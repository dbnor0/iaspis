{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Analysis.Utils where

import Control.Monad.State.Class
import Data.Text as T
import Lens.Micro.Platform
import Iaspis.Grammar
import Analysis.Environment
import Utils.Text
import Control.Monad.Error.Class
import Analysis.Error
import Data.Map as M
import Data.Maybe


type ScopeSetter = Lens' BuildInfo (Maybe Identifier)

withScope :: MonadState BuildEnv m => ScopeSetter -> Scope -> m a -> m ()
withScope t s f = do
  enterScope t s
  _ <- f
  exitScope t

enterScope :: MonadState BuildEnv m => ScopeSetter -> Scope -> m ()
enterScope setter s = do
  modify (setType . setScope)
  where setScope = (buildInfo . biScope) %~ updateScope s
        setType = (buildInfo . setter) ?~ s

exitScope :: MonadState BuildEnv m => ScopeSetter -> m ()
exitScope setter = do
  modify (setType . setScope)
  where setScope = (buildInfo . biScope) %~ revertScope
        setType = (buildInfo . setter) .~ Nothing

revertScope :: Scope -> Scope
revertScope = intercalate "::" . Prelude.init . splitOn "::"

enterBlock :: MonadState BuildEnv m => m ()
enterBlock = do
  modify $ (buildInfo . biDepth) +~ 1
  bd <- gets (^. (buildInfo . biDepth))
  modify $ (buildInfo . biScope) %~ updateScope (showT bd)

exitBlock :: MonadState BuildEnv m => m ()
exitBlock = do
  modify $ (buildInfo . biDepth) -~ 1
  modify $ (buildInfo . biScope) %~ revertScope

updateScope :: Identifier -> Scope -> Scope
updateScope id s
  | T.null s = id
  | otherwise = s <> "::" <> id

getField :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> m FieldEntry
getField id = do
  ls <- localScopes
  fs <- gets (^. fields)
  let entries = (\s -> M.lookup (s <> "::" <> id) fs) <$> ls
  if Prelude.all isNothing entries then
    throwError $ UndefinedId id
  else
    (return . Prelude.head . catMaybes) entries

getFn :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> m FunctionEntry
getFn id = do
  ls <- localScopes
  fs <- gets (^. functions)
  let entries = (\s -> M.lookup (s <> "::" <> id) fs) <$> ls
  if Prelude.all isNothing entries then
    throwError $ UndefinedId id
  else
    (return . Prelude.head . catMaybes) entries

getType :: MonadState BuildEnv m => MonadError BuildError m => Identifier -> m Type
getType id = do
  ts <- gets (^. types)
  case M.lookup id ts of
    Nothing -> throwError $ UndefinedType id
    Just t -> return t

localScopes :: MonadState BuildEnv m => m [Scope]
localScopes = do
  s <- gets (^. (buildInfo . biScope))
  return $ Prelude.scanl localScope s (Prelude.reverse [1..(scopeSize s)])
  where localScope t n = intercalate "::" $ Prelude.take n $ splitOn "::" t
        scopeSize s = Prelude.length $ splitOn "::" s
