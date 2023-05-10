{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Transpiler.Transpile.Facet where

import Transpiler.Iaspis.Grammar qualified as I
import Transpiler.Solidity.Grammar qualified as S
import Transpiler.Transpile.Types qualified as T
import Transpiler.Transpile.Common
import Transpiler.Iaspis.Grammar (Identifier)
import Transpiler.Transpile.Storage (qualifiedTypeId, storageGetterId, storageModuleId)
import Transpiler.Analysis.Scope
import Transpiler.Analysis.Environment
import Transpiler.Analysis.Utils (getFacetField, getFacet)
import Data.Set qualified as S
import Lens.Micro.Platform
import Control.Monad.State.Class
import Data.Maybe


transpileFacet :: BuildContext m => ([I.Import], I.Module, I.FacetContract) -> m T.Module
transpileFacet (is, m, f@I.FacetContract { I.facetName, I.proxyList }) = withScope m $ do
  tc <- transpileFacetContract f
  ce <- getFacet facetName
  return T.Module
    { T.moduleId = facetName
    , T.imports = proxyList : storageModuleId : unifyImports (ce ^. facetTypes)
    , T.decls = [tc]
    }
  where unifyImports ts = S.toList $ S.union (S.fromList (I.importIds =<< is)) ts


transpileFacetContract :: BuildContext m => I.FacetContract -> m T.Declaration
transpileFacetContract c@(I.FacetContract fId pId fns) = withScope c $ do
  tfns <- traverse (transpileFacetFn fId pId) fns
  return $ T.ContractDef (contractDef tfns)
  where contractDef = S.ContractDefinition False fId []

transpileFacetFn :: BuildContext m => Identifier -> Identifier -> I.Function -> m S.ContractBodyElem
transpileFacetFn fId pId fn@(I.Function hd body) = withScope fn $ do
  tbody <- traverse (transpileFacetStmt fId) body
  storageDecls <- storageStructDecls fId pId
  return $ S.FunctionDef $ S.FunctionDefinition
    { S.functionId = I.functionName hd
    , S.functionVisibility = transpileVisibility . Just $ I.functionVisibility hd
    , S.functionMutability = transpileMutability $ I.functionMutability hd
    , S.functionPayablity = transpilePayability $ I.functionPayability hd
    , S.functionVirtualSpec = False
    , S.functionOverrideSpec = False
    , S.functionArgs = transpileFunctionArg <$> I.functionArgs hd
    , S.functionReturnType = [transpileFunctionArg $ I.functionReturnType hd]
    , S.functionBody = storageDecls <> tbody
    }

storageStructId :: Identifier -> Identifier
storageStructId = (<> "Ds")

storageStructDecls :: BuildContext m => Identifier -> Identifier -> m [S.Statement]
storageStructDecls fId pId = do
  utils <- gets (^. storageUtils)
  return $ transpileStorageStructDecl pId <$> catMaybes 
    [ toMaybe (S.member fId utils) fId
    , toMaybe (S.member pId utils) pId
    ]
  where toMaybe False = const Nothing
        toMaybe True = Just

transpileStorageStructDecl :: Identifier -> Identifier -> S.Statement
transpileStorageStructDecl pId id = S.VarDeclStmt arg (Just expr)
  where arg = S.FunctionArg argType (storageStructId id)
        argType = S.PrimitiveT $ S.UserDefinedT (qualifiedTypeId pId id) (Just S.Storage)
        expr = S.FunctionCallE (S.IdentifierE $ storageGetterId pId id) []

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
    return $ S.MemberAccessE tlv (S.IdentifierE id)
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