{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Analysis.ContractCheck where

import Analysis.Environment.Environment
import Control.Monad.Error.Class
import Lens.Micro.Platform
import Data.Foldable
import Control.Monad
import Analysis.Environment.Error
import Control.Monad.State.Class
import Iaspis.Grammar hiding (facetList, proxyList)
import Data.Maybe
import Analysis.Environment.Utils


validProxyList :: (MonadState BuildEnv m, MonadError BuildError m) => m ()
validProxyList = do
  e <- gets (^. env)
  traverse_ (\p -> traverse_ (checkProxy e p) (facetList p)) (e ^. proxies)
  where checkProxy e p f = unless (f `elem` (facetId <$> e ^. facets)) (throwError $ UndefFacet (proxyId p) f)

validFacetList :: MonadState BuildEnv m => MonadError BuildError m => m ()
validFacetList = do
  e <- gets (^. env)
  traverse_ (\f -> traverse_ (checkFacet e f) (proxyList f)) (e ^. facets)
  where checkFacet e f p = unless (p `elem` (proxyId <$> e ^. proxies)) (throwError $ UndefProxy (facetId f) p)

validProxyMembers :: MonadState BuildEnv m => MonadError BuildError m => m ()
validProxyMembers = do
  be <- get
  e <- gets (^. env)
  let scopedPs = scopeProxy <$> e ^. proxies
      proxyDepth = 2
      pMembers = scopedPs >>= getTopLevelFields be proxyDepth
      checkPMember pMem = unless (isJust $ fieldProxyKind pMem)
        (throwError $ MissingProxyMemberKind (fieldName pMem))
  traverse_ checkPMember pMembers

scopeProxy :: ProxyEntry -> Scope
scopeProxy ProxyEntry { proxyId, proxyScope } = proxyScope <> "::" <> proxyId

checkContracts :: MonadState BuildEnv m => MonadError BuildError m =>  m ()
checkContracts = do
  validFacetList
  validProxyList
  validProxyMembers

