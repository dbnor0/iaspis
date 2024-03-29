{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Transpiler.Analysis.TypeCheck where

import Transpiler.Analysis.Environment
import Control.Monad.Error.Class
import Transpiler.Analysis.Error
import Transpiler.Iaspis.Grammar
import Data.Foldable
import Transpiler.Analysis.Utils
import Control.Monad
import Data.Text as T hiding (null, head, tail, zipWith, all, elem)
import Lens.Micro.Platform
import Transpiler.Iaspis.TypeUtils
import Transpiler.Analysis.Scope
import Control.Monad.State.Class
import Data.Map qualified as M
import Transpiler.Analysis.Module (detectCycles)

typeCheck :: BuildContext m => [Module] -> m ()
typeCheck ms = do
  checkRecursiveTypes
  traverse_ typeCheckModule ms

checkRecursiveTypes :: BuildContext m => m ()
checkRecursiveTypes = do
  ss <- gets (^. structs)
  let graph = M.map (\s -> name . structFieldType <$> structFields (s ^. structDef)) ss
  when (detectCycles graph) (throwError $ Debug "Recursive types are not allowed")

typeCheckModule :: BuildContext m => Module -> m ()
typeCheckModule m = withScope m $ traverse_ typeCheckDecl (declarations m)

typeCheckDecl :: BuildContext m => Declaration -> m ()
typeCheckDecl = \case
  ContractDecl c -> withScope c $ typeCheckContract c
  FacetDecl f -> withScope f $ typeCheckFacet f
  _ -> return ()

typeCheckContract :: BuildContext m => ImmutableContract -> m ()
typeCheckContract (ImmutableContract _ fields fns) = do
  traverse_ typeCheckField fields
  traverse_ typeCheckFn fns

typeCheckFacet :: BuildContext m => FacetContract -> m ()
typeCheckFacet (FacetContract _ _ fns) = traverse_ typeCheckFn fns

typeCheckField :: BuildContext m => Field -> m ()
typeCheckField Field{ fieldType, fieldName, fieldInitializer } =
  case fieldInitializer of
    Just init -> do
      t <- typeCheckExpr init
      unless (t == fieldType)
        (throwError $ InvalidAssigType fieldName fieldType t)
    Nothing -> return ()

typeCheckFn :: BuildContext m => Function -> m ()
typeCheckFn f@(Function hd stmts) = withScope f $ traverse_ (typeCheckStmt hd) stmts

typeCheckStmt :: BuildContext m => FunctionHeader -> Statement -> m ()
typeCheckStmt fn = \case
  VarDeclStmt f _ ex -> do
    bringInScope (declName f)
    field <- getField (declName f)
    et <- typeCheckExpr ex
    case typeLoc $ field ^. fdType of
      Just Storage -> do
        case typeLoc et of
          Just Storage -> do
            return ()
          _ -> throwError $ Debug "Storage pointers can only be initialized with storage values"
      _ -> do
        unless ((field ^. fdType) `strictEq` et)
          (throwError $ InvalidAssigType (field ^. fdId) (field ^. fdType) et)
  AssignmentStmt e _ ex -> do
    ft <- typeCheckExpr e
    et <- typeCheckExpr ex
    unless (ft `laxEq` et)
      (throwError $ InvalidAssigType "" ft et)
  ReturnStmt ex -> do
    et <- maybe (pure UnitT) typeCheckExpr ex
    unless ((argType . functionReturnType) fn `strictEq` et)
      (throwError $ InvalidReturnType (functionName fn) ((argType . functionReturnType) fn) et)
  IfStmt cond b1 b2 -> do
    enterBlock
    ct <- typeCheckExpr cond
    unless (ct == BoolT)
      (throwError $ InvalidExpressionType cond (Left BoolT) ct)
    typeCheckStmt fn b1
    exitBlock
    enterBlock
    maybe (return ()) (typeCheckStmt fn) b2
    exitBlock
  WhileStmt cond b -> do
    enterBlock
    ct <- typeCheckExpr cond
    unless (ct == BoolT)
      (throwError $ InvalidExpressionType cond (Left BoolT) ct)
    typeCheckStmt fn b
    exitBlock
  BlockStmt stmts -> do
    enterBlock
    traverse_ (typeCheckStmt fn) stmts
    exitBlock
  ExpressionStmt e -> void $ typeCheckExpr e
  _ -> return ()

typeCheckExpr :: BuildContext m => Expression -> m Type
typeCheckExpr = \case
  LiteralE l -> typeCheckLit l
  IdentifierE id -> do
    f <- getField id
    return $ f ^. fdType
  MemberAccessE e mem -> do
    t <- typeCheckExpr e
    case t of
      StructT _ _ -> do
        m <- getStructField t mem
        return $ structFieldType m
      _ -> throwError $ Debug "huh?"
  SubscriptE e idx -> do
    t <- typeCheckExpr e
    case t of
      ArrayT at ads al -> do
        tidx <- typeCheckExpr idx
        -- indexes must be integers for arrays
        unless (tidx `strictEq` UIntT) (throwError $ Debug "")
        case ads of
          [] -> throwError $ Debug ""
          [_] -> return (withLoc at al)
          _ -> return $ ArrayT at (tail ads) al
      MappingT t1 t2 -> do
        tidx <- typeCheckExpr idx
        unless (tidx `laxEq` t1) (throwError $ Debug $ "Expected " <> name t1 <> ", got " <> name tidx)
        return t2
      _ -> throwError $ Debug $ "Can't use subscript operator on type " <> name t
  FunctionCallE id args -> do
    fn <- getFn id
    ts <- traverse typeCheckExpr args
    traverse_ typeCheckArg (Prelude.zip (fn ^. fnArgs) ts)
    return . argType $ fn ^. fnReturn
    where typeCheckArg (arg, t) =
            unless (argType arg `strictEq` t)
              (throwError $ InvalidArgType arg (argType arg) t)
  UnaryE op e -> typeCheckUnaryExpr e op
  BinaryE op e1 e2 -> typeCheckBinaryExpr e1 e2 op
  InstantiationE tId _ -> getType tId

typeCheckLit :: BuildContext m => Value -> m Type
typeCheckLit = \case
  AddressV _ -> return AddressT
  BoolV _ -> return BoolT
  BytesV b ->
    if T.length b == 40 then
      return AddressT
    else
      return $ BytesT size
    where size = T.length b `div` 2
  UIntV _ -> return UIntT
  StringV _ -> return $ StringT (Just Memory)
  StructV StructValue{ structValueName, structValueMembers } -> do
    t <- getType structValueName
    case t of
      s@(StructT (Struct id fs) _) -> do
        let memIds = structMemberValueName <$> structValueMembers
            structIds = structFieldName <$> fs
            structTs = structFieldType <$> fs
        memTs <- traverse typeCheckExpr (structMemberValueExpr <$> structValueMembers)
        unless (memIds == structIds && and (zipWith laxEq memTs structTs))
          (throwError $ InvalidStructLiteral id fs structValueMembers)
        return s
      _ -> throwError InvalidStructType
  ArrayV es -> do
    when (null es) (throwError $ Debug "Empty arrays are not supported yet")
    tes <- traverse typeCheckExpr es
    unless (and $ fmap (head tes ==) tes) (throwError $ Debug "")
    case head tes of
      ArrayT t' ds' _ -> do
        return $ ArrayT t' (Just (Prelude.length es) : ds') (Just Memory)
      _ -> return $ ArrayT (head tes) [Just $ Prelude.length es] (Just Memory)
  EnumV e v -> do
    enum <- getFieldEnum e v
    case enum of
      Nothing -> throwError $ Debug ""
      Just e' -> return $ EnumT e'

typeCheckUnaryExpr :: BuildContext m => Expression -> UnaryOp -> m Type
typeCheckUnaryExpr e op = do
  t <- typeCheckExpr e
  case op of
    op
      | Data.Foldable.elem op numericUnaryOps ->
        unless (isNumeric t)
          (throwError $ InvalidExpressionType e (Right "numeric") t)
      | op == BitwiseNegationOp ->
        unless (isBitwise t)
          (throwError $ InvalidExpressionType e (Right "bitwise") t)
      | op == LogicalNegationOp ->
        unless (t == BoolT)
          (throwError $ InvalidExpressionType e (Left BoolT) t)
      | otherwise -> throwError InvalidOp
  return t

typeCheckBinaryExpr :: BuildContext m => Expression -> Expression -> BinaryOp -> m Type
typeCheckBinaryExpr e1 e2 op = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  case op of
    op
      | op `elem` arithOps || op `elem` relationalOps -> do
        unless (isNumeric t1)
          (throwError $ InvalidExpressionType e1 (Right "numeric") t1)
        unless (isNumeric t2)
          (throwError $ InvalidExpressionType e2 (Right "numeric") t2)
        return $ if op `elem` arithOps then t1 else BoolT
      | op `elem` eqOps -> do
        unless (t1 == t2)
          (throwError $ InvalidExpressionType e2 (Left t1) t2)
        return BoolT
      | op `elem` logicalOps -> do
        unless (t1 == BoolT)
          (throwError $ InvalidExpressionType e1 (Left BoolT) t1)
        unless (t2 == BoolT)
          (throwError $ InvalidExpressionType e2 (Left BoolT) t2)
        return t1
      | op `elem` bitwiseOps -> do
        unless (isBitwise t1)
          (throwError $ InvalidExpressionType e1 (Right "bitwise") t1)
        unless (isBitwise t2)
          (throwError $ InvalidExpressionType e2 (Right "bitwise") t2)
        return t1
      | otherwise -> throwError InvalidOp
