{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Transpiler.Transpile.Contract where

import Transpiler.Iaspis.Grammar qualified as I
import Transpiler.Transpile.Types qualified as T
import Transpiler.Solidity.Grammar qualified as S
import Data.Set qualified as S
import Transpiler.Transpile.Common
import Transpiler.Analysis.Environment
import Transpiler.Analysis.Utils (getContract)
import Lens.Micro.Platform
import Transpiler.Analysis.Scope

transpileContract :: BuildContext m => ([I.Import], I.Module, I.ImmutableContract) -> m T.Module
transpileContract (is, m, I.ImmutableContract { I.contractName, I.contractFields, I.contractFns }) = withScope m $ do
  ce <- getContract contractName
  return $ T.Module { T.moduleId=contractName, T.imports=unifyImports (ce ^. contractTypes), T.decls=[T.ContractDef contractDef] }
  where contractDef =
          S.ContractDefinition
          { S.contractAbstractSpec = False
          , S.contractId = contractName
          , S.contractInheritanceList = []
          , S.contractBody = (S.StateVarDecl . transpileContractField <$> contractFields) <> (fns <$> contractFns)
          }
        fns f@(I.Function (I.FunctionHeader _ _ _ "constructor" _ _ _) _) = (S.ConstructorDef . transpileFunctionDef) f
        fns f@(I.Function (I.FunctionHeader _ _ _ "fallback" _ _ _) _) = (S.FallbackDef . transpileFunctionDef) f
        fns f@(I.Function (I.FunctionHeader _ _ _ "receive" _ _ _) _) = (S.ReceiveDef . transpileFunctionDef) f
        fns f = (S.FunctionDef . transpileFunctionDef) f
        unifyImports ts = S.toList $ S.union (S.fromList (I.importIds =<< is)) ts

transpileContractField :: I.Field -> S.StateVarDeclaration
transpileContractField I.Field{ I.fieldType, I.fieldVisibility, I.fieldName, I.fieldInitializer } =
  S.StateVarDeclaration
    { S.stateVarType = transpileType fieldType
    , S.stateVarVisibility = transpileVisibility fieldVisibility
    , S.stateVarModifier = Nothing
    , S.stateVarId = fieldName
    , S.stateVarInitializer = transpileExpr <$> fieldInitializer
    }
