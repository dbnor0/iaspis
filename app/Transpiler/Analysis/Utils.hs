{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Transpiler.Analysis.Utils where

import Control.Monad.State.Class
import Data.Text as T hiding (scanl, reverse, take, length, filter, elem, find)
import Lens.Micro.Platform
import Transpiler.Iaspis.Grammar
import Transpiler.Analysis.Environment
import Control.Monad.Error.Class
import Transpiler.Analysis.Error
import Data.Map as M hiding (take, mapMaybe, filter)
import Data.Maybe
import Control.Monad.Writer
import Prelude hiding (Enum)
import Data.Foldable


getContract :: BuildContext m => Identifier -> m ContractEntry
getContract id = do
  ls <- localScopes
  cs <- gets (^. contracts)
  let entries = Data.Maybe.mapMaybe (\s -> M.lookup (s <> "::" <> id) cs) ls
  case entries of
    [] -> throwError $ UndefinedContract id
    c : _ -> return c

getFacet :: BuildContext m => Identifier -> m FacetEntry
getFacet id = do
  ls <- localScopes
  fs <- gets (^. facets)
  let entries = Data.Maybe.mapMaybe (\s -> M.lookup (s <> "::" <> id) fs) ls
  case entries of
    [] -> throwError $ UndefinedFacet id
    f : _ -> return f

getProxy :: BuildContext m => Identifier -> m ProxyEntry
getProxy id = do
  ls <- localScopes
  ps <- gets (^. proxies)
  let entries = Data.Maybe.mapMaybe (\s -> M.lookup (s <> "::" <> id) ps) ls
  case entries of
    [] -> throwError $ UndefinedId id
    p : _ -> return p

getField :: BuildContext m => Identifier -> m FieldEntry
getField id = do
  s <- gets (^. (buildInfo . biScope))
  ls <- localScopes
  fs <- gets (^. fields)
  case findFdInScope id ls fs of
    [] -> do
      ps <- proxyFieldsScope
      case findFdInScope id ps fs of
        [] -> throwError $ FieldNotInScope id s
        fd : _ -> do
          _ <- facetFieldCheck id
          unless (fd ^. fdInScope) (throwError $ FieldNotInScope id s)
          return fd
    fd : _ -> do
      unless (fd ^. fdInScope) (throwError $ FieldNotInScope id s)
      return fd

getFacetField :: BuildContext m => Identifier -> m (FieldEntry, Maybe Identifier)
getFacetField id = do
  s <- gets (^. (buildInfo . biScope))
  ls <- localScopes
  fs <- gets (^. fields)
  case findFdInScope id ls fs of
    [] -> do
      ps <- proxyFieldsScope
      case findFdInScope id ps fs of
        [] -> throwError $ FieldNotInScope id s
        fd : _ -> do
          fId <- facetFieldCheck id
          unless (fd ^. fdInScope) (throwError $ FieldNotInScope id s)
          return (fd, fId)
    fd : _ -> do
      unless (fd ^. fdInScope) (throwError $ FieldNotInScope id s)
      return (fd, Nothing)


findFdInScope :: Identifier -> [Scope] -> Bindings FieldEntry -> [FieldEntry]
findFdInScope id ss fs = filter (^. fdInScope) $ mapMaybe (lookupFn fs) ss
  where lookupFn fs "" = M.lookup id fs
        lookupFn fs s = M.lookup (s <> "::" <> id) fs

findFnInScope :: Identifier -> [Scope] -> Bindings FunctionEntry -> [FunctionEntry]
findFnInScope id ss fs = mapMaybe (lookupFn fs) ss
  where lookupFn fs "" = M.lookup id fs
        lookupFn fs s = M.lookup (s <> "::" <> id) fs


facetFieldCheck :: BuildContext m => Identifier -> m (Maybe Identifier)
facetFieldCheck id = do
  s <- gets (^. (buildInfo . biScope))
  fId <- gets (^. (buildInfo . biFacet))
  case fId of
    Just fId -> do
      f <- getFacet fId
      p <- getProxy (f ^. facetProxy)
      case find (\f -> fst f == id) (p ^. proxyFields) of
        Nothing -> throwError $ FieldNotInScope id s
        Just (_, Nothing) -> throwError $ FieldNotInScope id s
        Just (_, Just SharedProxyMember) -> return $ Just (p ^. proxyId)
        Just (_, Just (UniqueProxyMember f')) -> do
          unless (f' == fId) (throwError $ FieldNotInScope id s)
          return $ Just fId
    Nothing -> return Nothing

getFn :: BuildContext m => Identifier -> m FunctionEntry
getFn id = do
  ls <- localScopes
  fs <- gets (^. functions)
  case findFnInScope id ls fs of
    [] -> throwError $ UndefinedId id
    fd : _ -> return fd

getType :: BuildContext m => Identifier -> m Type
getType id = do
  ts <- gets (^. types)
  case M.lookup id ts of
    Nothing -> throwError $ UndefinedType id
    Just t -> return t

localScopes :: BuildContext m => m [Scope]
localScopes = do
  s <- gets (^. (buildInfo . biScope))
  return $ scanl localScope s (reverse [1..(scopeSize s)]) <> [""]
  where localScope t n = intercalate "::" $ take n $ splitOn "::" t
        scopeSize s = length $ splitOn "::" s

proxyFieldsScope :: BuildContext m => m [Scope]
proxyFieldsScope = do
  fs <- gets (^. (buildInfo . biFacet))
  case fs of
    Nothing -> return []
    Just fId -> do
      f <- getFacet fId
      p <- getProxy (f ^. facetProxy)
      return [(p ^. proxyScope) <> "::" <> (p ^. proxyId)]

getStructField :: BuildContext m => Type -> Identifier -> m StructField
getStructField t memId = do
  case t of
    StructT (Struct _ sMems) _ -> do
      let mem = find ((== memId) . structFieldName) sMems
      case mem of
        Nothing -> throwError $ Debug "1"
        Just m -> return m
    _ -> throwError $ Debug "2"

getFieldEnum :: BuildContext m => Identifier -> Identifier -> m (Maybe Enum)
getFieldEnum e id = do
  es <- gets (^. enums)
  unless (e `elem` M.keys es) (throwError $ Debug "")
  return $ view enumDef <$> find (\e -> id `elem` enumFields (e ^. enumDef)) es

getRootField :: BuildContext m => Expression -> m FieldEntry
getRootField (IdentifierE id) = getField id
getRootField (SubscriptE e _) = getRootField e
getRootField (MemberAccessE e _) = getRootField e
getRootField _ = throwError $ Debug "root field"
