{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpile.Storage where

import Iaspis.Grammar qualified as I
import Solidity.Grammar qualified as S
import Yul.Grammar qualified as Y
import Transpile.Types qualified as T
import Iaspis.DeclUtils
import Lens.Micro.Platform
import Data.List.Extra
import Data.Maybe
import Transpile.Utils
import Transpile.Common

type StorageElem = (S.StateVarDeclaration, S.StructDefinition, S.FunctionDefinition)

storageModule :: [([I.Import], I.ProxyContract, [Facet])] -> T.Module
storageModule ps =
  T.Module
  { T.imports = I.importIds =<< ((^. _1) =<< ps)
  , T.moduleId = "StorageStructs"
  , T.decls = [T.LibraryDef libraryDef]
  }
  where libraryDef = S.LibraryDefinition "StorageStructs" (storageModuleElems . (^. _2) =<< ps)

storageModuleElems :: I.ProxyContract -> [S.ContractBodyElem]
storageModuleElems (I.ProxyContract _ _ _ fs) = let x = toElems . storageTuple =<< fieldMappings in x
  where
    toElems = \(v, s, f) -> [S.StateVarDecl v, S.StructDef s, S.FunctionDef f]
    fieldMappings = groupSort (mapMaybe kindToFacetId fs)
    kindToFacetId f@I.Field{ I.fieldProxyKind } =
      case fieldProxyKind of
        Nothing -> Nothing
        (Just I.SharedProxyMember) -> Just ("global", f)
        (Just (I.UniqueProxyMember id)) -> Just (id, f)

storageTuple :: (S.Identifier, [I.Field]) -> StorageElem
storageTuple (sName, fs) = (ptrDecl, structDef, fnDef)
  where ptrDecl = S.StateVarDeclaration
                  { S.stateVarType = bytes 32
                  , S.stateVarVisibility = S.Public
                  , S.stateVarModifier = Just S.Constant
                  , S.stateVarId = ptrName
                  , S.stateVarInitializer = Just ptrExpr
                  }
        ptrName = sName <> "_storage_position"
        ptrExpr = S.FunctionCallE (S.IdentifierE "keccak256") [S.LiteralE (S.StringLit (sName <> "_storage_struct"))]
        structDef = S.StructDefinition (sName <> "_storage_struct") (structMember <$> fs)
        structMember I.Field{I.fieldName, I.fieldType} = S.StructMember (transpileType fieldType) fieldName
        fnDef = S.FunctionDefinition
                { S.functionId = sName <> "Storage"
                , S.functionVisibility = S.Internal
                , S.functionMutability = S.Pure
                , S.functionPayablity = False
                , S.functionVirtualSpec = False
                , S.functionOverrideSpec = False
                , S.functionArgs = []
                , S.functionReturnType = [S.FunctionArg (struct (sName <> "_storage_struct")) S.Storage "ds"]
                , S.functionBody = fnBody
                }
        fnBody = [ S.VarDeclStmt (S.FunctionArg (bytes 32) S.Memory "position") (Just (S.IdentifierE ptrName))
                 , S.AssemblyStmt (Y.AssignmentStmt (Y.PathE (Y.IdentifierE "ds") (Y.IdentifierE "slot")) (Y.IdentifierE "position"))
                 ]
