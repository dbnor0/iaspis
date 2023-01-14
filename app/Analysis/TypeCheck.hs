{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.TypeCheck where

import Control.Monad.State.Class
import Control.Monad.Error.Class
import Analysis.Environment.Error
import Iaspis.Grammar
import Data.Foldable
import Control.Monad
import Utils.Text
import Lens.Micro.Platform
import Iaspis.TypeUtils
import Analysis.Environment.Environment
import Analysis.Environment.Utils
import Analysis.Environment.Traversals


typeCheck :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
typeCheck m@Module{ declarations } = traverseModule m $ traverse_ typeCheckDecl declarations

typeCheckDecl :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
typeCheckDecl = \case
  ContractDecl c -> typeCheckContract c

typeCheckContract :: MonadState BuildEnv m => MonadError BuildError m => Contract -> m ()
typeCheckContract = traverseContract cFn pFn fFn
  where cFn = \case
          ImmutableContract _ _ fns -> traverse_ typeCheckFn fns
          _ -> return ()
        pFn = const $ return ()
        fFn = \case
          FacetContract _ _ fns -> do
            traverse_ typeCheckFn fns
          _ -> return ()

typeCheckFn :: MonadState BuildEnv m => MonadError BuildError m => Function -> m ()
typeCheckFn f@(Function hd stmts) = traverseFn f $ traverse_ (typeCheckStmt hd) stmts

typeCheckStmt :: MonadState BuildEnv m => MonadError BuildError m => FunctionHeader -> Statement -> m ()
typeCheckStmt fn = \case
  VarDeclStmt f _ ex -> do
    et <- typeCheckExpr ex
    unless (fieldType f == et)
      (throwError $ InvalidAssigType (fieldName f) (fieldType f) et)
  AssignmentStmt (IdentifierE fId) _ ex -> do
    f <- getField fId
    et <- typeCheckExpr ex
    unless (fieldType f == et)
      (throwError $ InvalidAssigType (fieldName f) (fieldType f) et)
  ReturnStmt ex -> do
    et <- maybe (pure UnitT) typeCheckExpr ex
    unless (functionReturnType fn == et)
      (throwError $ InvalidReturnType (functionName fn) (functionReturnType fn) et)
  IfStmt cond b1 b2 -> do
    ct <- typeCheckExpr cond
    unless (ct == BoolT)
      (throwError $ InvalidExpressionType cond (Left BoolT) ct)
    typeCheckStmt fn b1
    maybe (return ()) (typeCheckStmt fn) b2
  BlockStmt stmts -> do
    bs <- gets (showT . (^. blockDepth))
    withScope bs $ traverse_ (typeCheckStmt fn) stmts
  ExpressionStmt e -> void $ typeCheckExpr e
  _ -> return ()

typeCheckExpr :: MonadState BuildEnv m => MonadError BuildError m => Expression -> m Type
typeCheckExpr = \case
  LiteralE l -> return $ typeCheckLit l
  IdentifierE id -> do
    f <- getField id 
    return $ fieldType f
  FunctionCallE id args -> do
    fn <- getFn id
    ts <- traverse typeCheckExpr args
    traverse_ typeCheckArg (zip (functionArgs fn) ts)
    return $ functionReturnType fn
    where typeCheckArg (arg, t) = 
            unless (fieldType arg == t) 
              (throwError $ InvalidArgType arg (fieldType arg) t)
  UnaryE op e -> typeCheckUnaryExpr e op
  BinaryE op e1 e2 -> typeCheckBinaryExpr e1 e2 op
  InstantiationE tId _ -> getType tId

typeCheckLit :: Value -> Type
typeCheckLit = \case
  AddressV _ -> AddressT
  BoolV _ -> BoolT
  BytesV _ -> BytesDynamicT
  UIntV _ -> UIntT 256
  StringV _ -> StringT

typeCheckUnaryExpr :: MonadState BuildEnv m => MonadError BuildError m => Expression -> UnaryOp -> m Type
typeCheckUnaryExpr e op = do
  t <- typeCheckExpr e
  case op of
    op
      | op `elem` numericUnaryOps ->
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

typeCheckBinaryExpr :: MonadState BuildEnv m => MonadError BuildError m => Expression -> Expression -> BinaryOp -> m Type
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
        return t1
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

