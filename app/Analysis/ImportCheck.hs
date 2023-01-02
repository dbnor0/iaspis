{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analysis.ImportCheck where

import Control.Monad.Error.Class
import Control.Monad.State.Class
import Iaspis.Grammar
import Lens.Micro.Platform
import Data.Foldable
import Analysis.Environment.Environment
import Control.Monad
import Analysis.Environment.Error
import Analysis.Environment.Utils (isTopLevelEntry)


checkImports :: MonadError BuildError m => MonadState BuildEnv m => Module -> m ()
checkImports (Module m imports _) = do
  e <- gets (^. env)
  traverse_ (checkImport (e ^. modules)) (importModule <$> imports)
  traverse_ (checkImportList m) imports
  where checkImport ms i = unless (i `elem` (moduleId <$> ms)) (throwError $ UndefImport i m)

checkImportList :: MonadError BuildError m => MonadState BuildEnv m => Identifier -> Import -> m ()
checkImportList id Import{ importIds, importModule } = traverse_ (checkImport id importModule) importIds
  where checkImport id m i = do
          c <- isTopLevelEntry i m
          unless c (throwError $ UndefExport id i m)