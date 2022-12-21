{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}

module Analysis.ContractCheck where

import Analysis.Environment.Environment
import Control.Monad.Error.Class
import Lens.Micro.Platform
import Data.Foldable
import Control.Monad
import Analysis.Environment.Error
import Control.Monad.State.Class
import Analysis.Environment.Build


validProxyList :: (MonadState BuildEnv m, MonadError BuildError m) => m ()
validProxyList = do
  e <- gets (^. env)
  traverse_ (\p -> traverse_ (checkProxy e p) (facetList p)) (e ^. proxies)
  where checkProxy e p f = unless (f `elem` (facetId <$> e ^. facets)) (throwError $ UndefFacet (proxyId p) f)

validFacetList :: (MonadState BuildEnv m, MonadError BuildError m) => m ()
validFacetList = do
  e <- gets (^. env)
  traverse_ (\f -> traverse_ (checkFacet e f) (proxyList f)) (e ^. facets)
  where checkFacet e f p = unless (p `elem` (proxyId <$> e ^. proxies)) (throwError $ UndefProxy (facetId f) p)

checkContracts :: (MonadState BuildEnv m, MonadError BuildError m) =>  m ()
checkContracts = do
  validFacetList
  validProxyList