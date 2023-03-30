{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Transpile.Facet where

import Iaspis.Grammar qualified as I
import Solidity.Grammar qualified as S
import Transpile.Types qualified as T
import Transpile.Common
import Iaspis.Grammar (Identifier)
import Transpile.Storage (qualifiedTypeId, storageGetterId, storageModuleId, sharedStorageId)
import Analysis.Scope
import Analysis.Environment
import Analysis.Utils (getFacetField)


transpileFacet :: BuildContext m => ([I.Import], I.Module, I.FacetContract) -> m T.Module
transpileFacet (is, m, f@I.FacetContract { I.facetName, I.proxyList }) = withScope m $ do
  tc <- transpileFacetContract f
  return T.Module
    { T.moduleId = facetName
    , T.imports = proxyList : storageModuleId : (I.importIds =<< is)
    , T.decls = [tc]
    }

transpileFacetContract :: BuildContext m => I.FacetContract -> m T.Declaration
transpileFacetContract c@(I.FacetContract fId _ fns) = withScope c $ do
  tfns <- traverse (transpileFacetFn fId) fns
  return $ T.ContractDef (contractDef tfns)
  where contractDef = S.ContractDefinition False fId []

transpileFacetFn :: BuildContext m => Identifier -> I.Function -> m S.ContractBodyElem
transpileFacetFn fId fn@(I.Function hd body) = withScope fn $ do
  tbody <- traverse (transpileFacetStmt fId) body
  return $ S.FunctionDef $ S.FunctionDefinition
    { S.functionId = I.functionName hd
    , S.functionVisibility = transpileVisibility . Just $ I.functionVisibility hd
    , S.functionMutability = transpileMutability $ I.functionMutability hd
    , S.functionPayablity = transpilePayability $ I.functionPayability hd
    , S.functionVirtualSpec = False
    , S.functionOverrideSpec = False
    , S.functionArgs = transpileFunctionArg <$> I.functionArgs hd
    , S.functionReturnType = [transpileFunctionArg $ I.functionReturnType hd]
    , S.functionBody = storageStructDecls fId <> tbody
    }

storageStructId :: Identifier -> Identifier
storageStructId = (<> "Ds")

storageStructDecls :: Identifier -> [S.Statement]
storageStructDecls pId =
  [ transpileStorageStructDecl pId
  , transpileStorageStructDecl sharedStorageId
  ]

transpileStorageStructDecl :: Identifier -> S.Statement
transpileStorageStructDecl id = S.VarDeclStmt arg (Just expr)
  where arg = S.FunctionArg argType (storageStructId id)
        argType = S.PrimitiveT $ S.UserDefinedT (qualifiedTypeId id) (Just S.Storage)
        expr = S.FunctionCallE (S.IdentifierE $ storageGetterId id) []

transpileFacetStmt :: BuildContext m => Identifier -> I.Statement -> m S.Statement
transpileFacetStmt fId = \case
  I.VarDeclStmt f _ e -> do
    bringInScope (I.declName f)
    te <- transpileFacetExpr fId e
    return $ S.VarDeclStmt (transpileDeclArg f) (Just te)
  I.AssignmentStmt id _ e -> do
    tid <- transpileFacetExpr fId id
    te <- transpileFacetExpr fId e
    return $ S.AssignmentStmt tid te
  I.ReturnStmt e -> do
    te <- traverse (transpileFacetExpr fId) e
    return $ S.ReturnStmt te
  I.IfStmt cond b1 b2 -> do
    tcond <- transpileFacetExpr fId cond
    enterBlock
    tb1 <- transpileFacetStmt fId b1
    exitBlock
    enterBlock
    tb2 <- traverse (transpileFacetStmt fId) b2
    exitBlock
    return $ S.IfStmt tcond tb1 tb2
  I.WhileStmt cond b -> do
    enterBlock
    tcond <- transpileFacetExpr fId cond
    tb <- transpileFacetStmt fId b
    exitBlock
    return $ S.WhileStmt tcond tb
  I.BlockStmt stmts -> do
    enterBlock
    tstmts <- traverse (transpileFacetStmt fId) stmts
    exitBlock
    return $ S.BlockStmt tstmts
  I.BreakStmt -> return S.BreakStmt
  I.ContinueStmt -> return S.ContinueStmt
  I.ExpressionStmt e -> do
    te <- transpileFacetExpr fId e
    return $ S.ExpressionStmt te

transpileFacetExpr :: BuildContext m => Identifier -> I.Expression -> m S.Expression
transpileFacetExpr fId = \case
  I.LiteralE v -> return $ S.LiteralE $ transpileValue v
  I.IdentifierE id -> do
    (_, facet) <- getFacetField id
    case facet of
      Nothing -> return $ S.IdentifierE id
      Just f -> return $ S.MemberAccessE (S.IdentifierE (storageStructId f)) (S.IdentifierE id)
  I.SubscriptE e idx -> do
    te <- transpileFacetExpr fId e
    tidx <- transpileFacetExpr fId idx
    return $ S.SubscriptE te tidx
  I.MemberAccessE lv id -> do
    tlv <- transpileFacetExpr fId lv
    tid <- transpileFacetExpr fId $ I.IdentifierE id
    return $ S.MemberAccessE tlv tid
  I.FunctionCallE id es -> do
    tes <- traverse (transpileFacetExpr fId) es
    return $ S.FunctionCallE (S.IdentifierE id) tes
  I.InstantiationE id es -> do
    tes <- traverse (transpileFacetExpr fId) es
    return $ S.InstantiationE (S.IdentifierE id) tes
  I.UnaryE op e -> do
    te <- transpileFacetExpr fId e
    return $ S.UnaryE (transpileUnaryOp op) te
  I.BinaryE op e1 e2 -> do
    te1 <- transpileFacetExpr fId e1
    te2 <- transpileFacetExpr fId e2
    return $ S.BinaryE (transpileBinaryOp op) te1 te2