{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Analysis.Build.Contract where

import Control.Monad.State.Class
import Analysis.Environment.AltEnvironment
import Control.Monad.Error.Class
import Analysis.Build.Error
import Data.Map as M
import Lens.Micro.Platform
import Control.Monad
import Data.Foldable

contractChecks :: MonadState BuildEnv m => MonadError BuildError m => m ()
contractChecks = do
  ps <- gets (M.elems . (^. proxies))
  traverse_ proxyChecks ps

proxyChecks :: MonadState BuildEnv m => MonadError BuildError m => ProxyEntry -> m ()
proxyChecks (ProxyEntry _ facetList _) = do
  fs <- gets (M.elems . (^. facets))
  unless (all (`elem` (view facetId <$> fs)) facetList) (throwError InvalidFacets)

facetChecks :: MonadState BuildEnv m => MonadError BuildError m => FacetEntry -> m ()
facetChecks (FacetEntry _ facetProxy _) = do
  ps <- gets (M.elems . (^. proxies))
  unless (facetProxy `elem` (view proxyId <$> ps)) (throwError InvalidProxy)