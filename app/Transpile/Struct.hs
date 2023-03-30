{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Transpile.Struct where

import Iaspis.Grammar qualified as I
import Solidity.Grammar qualified as S
import Transpile.Types qualified as T
import Transpile.Common
import Data.Maybe
import Analysis.Environment

transpileStruct :: BuildContext m => I.Struct -> m T.Module
transpileStruct s@I.Struct{ I.structName, I.structFields } = 
  return T.Module
    { T.imports = deps
    , T.moduleId = structName
    , T.decls = [T.StructTypeDef $ transpileStructDecl s]
    }
  where depId (I.UserDefinedT id _) = Just id
        depId _ = Nothing
        deps = mapMaybe (depId . I.structFieldType) structFields

transpileStructDecl :: I.Struct -> S.StructDefinition
transpileStructDecl (I.Struct id fs) = S.StructDefinition id ( transpileStructMember <$> fs)

transpileStructMember :: I.StructField -> S.StructMember
transpileStructMember (I.StructField t id) = S.StructMember (transpileType t) id






