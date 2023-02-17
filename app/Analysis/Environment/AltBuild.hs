{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Analysis.Environment.AltBuild where

import Control.Monad.State.Class
import Analysis.Environment.AltEnvironment
import Control.Monad.Error.Class
import Iaspis.Grammar
import Control.Monad
import Lens.Micro.Platform
import Data.Map as M


data BuildError
  = DupModule Identifier

addModules :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
addModules Module{ moduleDecl, imports, declarations } = do
  ms <- gets (M.elems . (^. modules))
  uniqueId moduleDecl (_moduleId <$> ms) DupModule
  modify $ modules %~ M.insert moduleDecl entry
  where entry = ModuleEntry moduleDecl imports (declId <$> declarations)
        declId (ContractDecl c) = contractName c
        declId (ProxyDecl p) = proxyName p
        declId (FacetDecl f) = facetName f

uniqueId :: MonadError BuildError m
         => Identifier
         -> [Identifier]
         -> (Identifier -> BuildError)
         -> m ()
uniqueId id env err = when (id `elem` env) (throwError $ err id)
