{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpile.Contract where

import Iaspis.Grammar qualified as I
import Transpile.Types qualified as T
import Solidity.Grammar qualified as S
import Transpile.Common

transpileContract :: ([I.Import], I.ImmutableContract) -> T.Module
transpileContract (is, I.ImmutableContract { I.contractName, I.contractFields, I.contractFns }) =
  T.Module { T.moduleId=contractName, T.imports=I.importIds =<< is, T.decls=[T.ContractDef contractDef] }
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

transpileContractField :: I.Field -> S.StateVarDeclaration
transpileContractField I.Field{ I.fieldType, I.fieldVisibility, I.fieldName, I.fieldInitializer } =
  S.StateVarDeclaration
    { S.stateVarType = transpileType fieldType
    , S.stateVarVisibility = transpileVisibility fieldVisibility
    , S.stateVarModifier = Nothing
    , S.stateVarId = fieldName
    , S.stateVarInitializer = transpileExpr <$> fieldInitializer
    }
