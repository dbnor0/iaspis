{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Analysis.Contract where

import Control.Monad.State.Class
import Analysis.Environment
import Control.Monad.Error.Class
import Analysis.Error
import Data.Map as M
import Lens.Micro.Platform
import Control.Monad
import Data.Foldable
import Iaspis.Grammar

contractChecks :: BuildContext m => [Module] -> m ()
contractChecks ms = traverse_ declContractChecks (declarations =<< ms)

declContractChecks :: BuildContext m => Declaration -> m ()
declContractChecks = \case
  ProxyDecl p -> do
    proxyChecks p
    proxyFieldChecks p
  FacetDecl f -> do
    facetChecks f
  _ -> return ()

proxyChecks :: BuildContext m => ProxyContract -> m ()
proxyChecks ProxyContract{ facetList } = do
  fs <- gets (M.elems . (^. facets))
  unless (all (`elem` (view facetId <$> fs)) facetList) (throwError InvalidFacets)

facetChecks :: BuildContext m => FacetContract -> m ()
facetChecks FacetContract{ proxyList } = do
  ps <- gets (M.elems . (^. proxies))
  unless (proxyList `elem` (view proxyId <$> ps)) (throwError InvalidProxy)

proxyFieldChecks :: BuildContext m => ProxyContract -> m ()
proxyFieldChecks ProxyContract{ proxyDecls, facetList } = traverse_ checkField proxyDecls
  where checkField Field{ fieldName, fieldProxyKind } = 
          unless (checkProxyKind fieldProxyKind) (throwError $ InvalidProxyField fieldName)
        checkProxyKind Nothing = False
        checkProxyKind (Just SharedProxyMember) = True 
        checkProxyKind (Just (UniqueProxyMember fId)) = fId `elem` facetList