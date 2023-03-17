{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Analysis.Utils where

import Control.Monad.State.Class
import Data.Text as T
import Lens.Micro.Platform
import Iaspis.Grammar
import Analysis.Environment
import Control.Monad.Error.Class
import Analysis.Error
import Data.Map as M
import Data.Maybe
import Data.Foldable qualified
import Control.Monad (unless)

getField :: BuildContext m => Identifier -> m FieldEntry
getField id = do
  ls <- localScopes
  fs <- gets (^. fields)
  let entries = Data.Maybe.mapMaybe (\s -> M.lookup (s <> "::" <> id) fs) ls
  case entries of
    [] -> throwError $ UndefinedId id
    fd : _ -> do
      s <- gets (^. (buildInfo . biScope))
      unless (fd ^. fdInScope) (throwError $ FieldNotInScope id s)
      return fd

getFn :: BuildContext m => Identifier -> m FunctionEntry
getFn id = do
  ls <- localScopes
  fs <- gets (^. functions)
  let entries = Data.Maybe.mapMaybe (\s -> M.lookup (s <> "::" <> id) fs) ls
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
  return $ Prelude.scanl localScope s (Prelude.reverse [1..(scopeSize s)])
  where localScope t n = intercalate "::" $ Prelude.take n $ splitOn "::" t
        scopeSize s = Prelude.length $ splitOn "::" s

getStructField :: BuildContext m => Type -> Identifier -> m StructField
getStructField t memId = do
  case t of
    StructT (Struct _ sMems) -> do
      let mem = Data.Foldable.find ((== memId) . structFieldName) sMems
      case mem of
        Nothing -> throwError NotYetImplemented
        Just m -> return m
    _ -> throwError NotYetImplemented