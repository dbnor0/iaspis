{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpiler.Transpile.Storage where

import Transpiler.Iaspis.Grammar qualified as I
import Transpiler.Solidity.Grammar qualified as S
import Transpiler.Yul.Grammar qualified as Y
import Transpiler.Transpile.Types qualified as T
import Transpiler.Iaspis.DeclUtils
import Lens.Micro.Platform
import Data.List.Extra
import Data.Maybe
import Transpiler.Transpile.Utils
import Transpiler.Transpile.Common
import Transpiler.Solidity.Grammar (Identifier)

type StorageElem = (S.StateVarDeclaration, S.StructDefinition, S.FunctionDefinition)

qualifiedTypeId :: Identifier -> Identifier
qualifiedTypeId id = "StorageStructs." <> storageStructId id

storageModuleId :: Identifier
storageModuleId = "StorageStructs"

storageStructId :: Identifier -> Identifier
storageStructId = (<> "_storage_struct")

storageGetterId :: Identifier -> Identifier
storageGetterId id = "StorageStructs." <> id <> "Storage"

storageModule :: [([I.Import], I.ProxyContract, I.Module, [Facet])] -> T.Module
storageModule ps =
  T.Module
  { T.imports = I.importIds =<< ((^. _1) =<< ps)
  , T.moduleId = storageModuleId
  , T.decls = [T.LibraryDef libraryDef]
  }
  where libraryDef = S.LibraryDefinition storageModuleId (storageModuleElems . (^. _2) =<< ps)

storageModuleElems :: I.ProxyContract -> [S.ContractBodyElem]
storageModuleElems (I.ProxyContract _ pId _ fs) = toElems . storageTuple =<< fieldMappings
  where
    toElems = \(v, s, f) -> [S.StateVarDecl v, S.StructDef s, S.FunctionDef f]
    fieldMappings = groupSort (mapMaybe kindToFacetId fs)
    kindToFacetId f@I.Field{ I.fieldProxyKind } =
      case fieldProxyKind of
        Nothing -> Nothing
        (Just I.SharedProxyMember) -> Just (pId, f)
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
        ptrExpr = S.FunctionCallE (S.IdentifierE "keccak256") [S.LiteralE (S.StringLit (storageStructId sName))]
        structDef = S.StructDefinition (storageStructId sName) (structMember <$> fs)
        structMember I.Field{I.fieldName, I.fieldType} = S.StructMember (transpileType fieldType) fieldName
        fnDef = S.FunctionDefinition
                { S.functionId = sName <> "Storage"
                , S.functionVisibility = S.Internal
                , S.functionMutability = S.Pure
                , S.functionPayablity = False
                , S.functionVirtualSpec = False
                , S.functionOverrideSpec = False
                , S.functionArgs = []
                , S.functionReturnType = [S.FunctionArg (struct (storageStructId sName) (Just S.Storage)) "ds"]
                , S.functionBody = fnBody
                }
        fnBody = [ S.VarDeclStmt (S.FunctionArg (bytes 32) "position") (Just (S.IdentifierE ptrName))
                 , S.AssemblyStmt (Y.AssignmentStmt (Y.PathE (Y.IdentifierE "ds") (Y.IdentifierE "slot")) (Y.IdentifierE "position"))
                 ]
