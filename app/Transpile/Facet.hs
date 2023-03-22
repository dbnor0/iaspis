{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpile.Facet where

import Iaspis.Grammar qualified as I
import Solidity.Grammar qualified as S
import Transpile.Types qualified as T
import Transpile.Common
import Transpile.Utils


transpileFacet :: ([I.Import], I.FacetContract) -> Maybe T.Module
transpileFacet (is, f@I.FacetContract { I.facetName, I.proxyList }) =
  Just $ T.Module
    { T.moduleId=facetName
    , T.imports=(I.importIds =<< is) <> [proxyList]
    , T.decls=transpileFacetContract f
    }

transpileFacetContract :: I.FacetContract -> [T.Declaration]
transpileFacetContract (I.FacetContract fId pId fns) = [T.ContractDef contractDef]
  where contractDef = S.ContractDefinition False fId [] (transpileFacetFn <$> fns)

transpileFacetFn :: I.Function -> S.ContractBodyElem
transpileFacetFn (I.Function hd body) = S.FunctionDef $ S.FunctionDefinition
  { S.functionId = I.functionName hd
  , S.functionVisibility = transpileVisibility . Just $ I.functionVisibility hd
  , S.functionMutability = transpileMutability $ I.functionMutability hd
  , S.functionPayablity = transpilePayability $ I.functionPayability hd
  , S.functionVirtualSpec = False
  , S.functionOverrideSpec = False
  , S.functionArgs = []
  , S.functionReturnType = [S.FunctionArg unit S.Memory ""]
  , S.functionBody = [S.NoOpStmt]
  }
