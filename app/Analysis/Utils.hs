{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Analysis.Utils where

import Control.Monad.State.Class
import Data.Text as T hiding (scanl, reverse, take, length, filter, elem, find)
import Lens.Micro.Platform
import Iaspis.Grammar
import Analysis.Environment
import Control.Monad.Error.Class
import Analysis.Error
import Data.Map as M hiding (take, mapMaybe, filter)
import Data.Maybe
import Control.Monad.Writer
import Prelude hiding (Enum)
import Data.Foldable

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
  case findInScope id ls fs of
    [] -> do
      ps <- proxyFieldsScope
      case findInScope id ps fs of
        [] -> throwError $ FieldNotInScope id s
        fd : _ -> do
          facetFieldCheck id
          unless (fd ^. fdInScope) (throwError $ FieldNotInScope id s)
          return fd
    fd : _ -> do
      unless (fd ^. fdInScope) (throwError $ FieldNotInScope id s)
      return fd

findInScope :: Identifier -> [Scope] -> Bindings FieldEntry -> [FieldEntry]
findInScope id ss fs = filter (^. fdInScope) $ mapMaybe (\s -> M.lookup (s <> "::" <> id) fs) ss

facetFieldCheck :: BuildContext m => Identifier -> m ()
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
        Just (_, Just (UniqueProxyMember f')) -> do
          unless (f' == fId) (throwError $ FieldNotInScope id s)
        _ -> return ()
    Nothing -> return ()

getFn :: BuildContext m => Identifier -> m FunctionEntry
getFn id = do
  ls <- localScopes
  fs <- gets (^. functions)
  let entries = mapMaybe (\s -> M.lookup (s <> "::" <> id) fs) ls
  case entries of
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
  return $ scanl localScope s (reverse [1..(scopeSize s)])
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
    StructT (Struct _ sMems) -> do
      let mem = find ((== memId) . structFieldName) sMems
      case mem of
        Nothing -> throwError NotYetImplemented
        Just m -> return m
    _ -> throwError NotYetImplemented

getFieldEnum :: BuildContext m => Identifier -> m (Maybe Enum)
getFieldEnum id = do
  es <- gets (^. enums)
  return $ view enumDef <$> find (\e -> id `elem` enumFields (e ^. enumDef)) es