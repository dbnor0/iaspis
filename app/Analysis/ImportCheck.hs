{-# LANGUAGE FlexibleContexts #-}

module Analysis.ImportCheck where

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Iaspis.Grammar
import Lens.Micro.Platform
import Data.Foldable
import Analysis.Environment.Environment
import Control.Monad
import Analysis.Environment.Error


checkImports :: MonadError BuildError m => MonadState BuildEnv m => Module -> m ()
checkImports (Module m imports _) = do
  e <- gets (^. env)
  traverse_ (checkImport (e ^. modules)) imports
  where checkImport ms i = unless (i `elem` (moduleId <$> ms)) (throwError $ UndefImport i m)
