-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE NamedFieldPuns #-}

-- module Analysis.Build.Contract where

-- import Control.Monad.State.Class
-- import Analysis.Environment.AltEnvironment
-- import Control.Monad.Error.Class
-- import Analysis.Build.Error
-- import Iaspis.Grammar


-- buildModules :: MonadState BuildEnv m => MonadError BuildError m => [Module] -> m ()
-- buildModules ms = do
--   traverse_ addContracts ms

-- addContracts :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
-- addContracts Module{ moduleDecl, imports, declarations } = do
--   ms <- gets (M.elems . (^. modules))
--   uniqueId moduleDecl (view moduleId <$> ms) DupModule
--   modify $ modules %~ M.insert moduleDecl entry
--   where entry = ModuleEntry moduleDecl imports (declId <$> declarations) []
