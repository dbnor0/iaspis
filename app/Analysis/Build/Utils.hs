{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Analysis.Build.Utils where

import Control.Monad.State.Class
import Data.Text as T
import Lens.Micro.Platform
import Iaspis.Grammar
import Analysis.Environment.AltEnvironment


type ScopeSetter = Lens' BuildInfo (Maybe Identifier)

withScope :: MonadState BuildEnv m => ScopeSetter -> Scope -> m a -> m ()
withScope t s f = do
  enterScope t s
  _ <- f
  exitScope t

enterScope :: MonadState BuildEnv m => ScopeSetter -> Scope -> m ()
enterScope setter s = modify (setType . setScope)
  where setScope = (buildInfo . biScope) %~ updateScope s
        setType = (buildInfo . setter) ?~ s

exitScope :: MonadState BuildEnv m => ScopeSetter -> m ()
exitScope setter = modify (setType . setScope)
  where setScope = (buildInfo . biScope) %~ revertScope
        setType = (buildInfo . setter) .~ Nothing

revertScope :: Scope -> Scope
revertScope = intercalate "::" . Prelude.init . splitOn "::"

updateScope :: Identifier -> Scope -> Scope
updateScope id s
  | T.null s = id
  | otherwise = s <> "::" <> id
