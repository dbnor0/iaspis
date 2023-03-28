{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpile.Facet where

import Iaspis.Grammar qualified as I
import Solidity.Grammar qualified as S
import Transpile.Types qualified as T
import Transpile.Common
import Iaspis.Grammar (Identifier)
import Transpile.Storage (qualifiedTypeId, storageGetterId, storageModuleId, sharedStorageId)


transpileFacet :: ([I.Import], I.FacetContract) -> T.Module
transpileFacet (is, f@I.FacetContract { I.facetName, I.proxyList }) =
  T.Module
    { T.moduleId=facetName
    , T.imports=proxyList : storageModuleId : (I.importIds =<< is)
    , T.decls=transpileFacetContract f
    }

transpileFacetContract :: I.FacetContract -> [T.Declaration]
transpileFacetContract (I.FacetContract fId pId fns) = [T.ContractDef contractDef]
  where contractDef = S.ContractDefinition False fId [] (transpileFacetFn fId <$> fns)

transpileFacetFn :: Identifier -> I.Function -> S.ContractBodyElem
transpileFacetFn fId (I.Function hd body) = S.FunctionDef $ S.FunctionDefinition
  { S.functionId = I.functionName hd
  , S.functionVisibility = transpileVisibility . Just $ I.functionVisibility hd
  , S.functionMutability = transpileMutability $ I.functionMutability hd
  , S.functionPayablity = transpilePayability $ I.functionPayability hd
  , S.functionVirtualSpec = False
  , S.functionOverrideSpec = False
  , S.functionArgs = transpileFunctionArg <$> I.functionArgs hd
  , S.functionReturnType = [transpileFunctionArg $ I.functionReturnType hd]
  , S.functionBody = storageStructDecls fId <> (transpileStmt <$> body)
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
  where arg = S.FunctionArg argType  S.Storage (storageStructId id)
        argType = S.PrimitiveT $ S.UserDefinedT (qualifiedTypeId id)
        expr = S.FunctionCallE (S.IdentifierE $ storageGetterId id) []