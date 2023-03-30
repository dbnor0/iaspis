{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Analysis.Scope where

import Analysis.Environment
import Iaspis.Grammar qualified as I
import Control.Monad.State.Class
import Lens.Micro.Platform
import Data.Map qualified as M
import Data.Foldable
import Iaspis.Grammar (Identifier)
import Data.Text qualified as T
import Utils.Text
import Analysis.Utils (getProxy, getFacet)
import Control.Monad.Writer


type ScopeSetter = Lens' BuildInfo (Maybe Identifier)

class HasScope a where
  scopeId :: a -> Identifier

  scopeSetter :: a -> ScopeSetter

  scopeDecls :: a -> [Identifier]
  scopeDecls _ = []

  withScope :: BuildContext m => a -> m b -> m b
  withScope e f = do
    enterScope e
    b <- f
    exitScope e
    return b

  enterScope :: BuildContext m  => a -> m ()
  enterScope e = do
    modify (setType . setScope)
    traverse_ (\id -> when (id /= "") (bringInScope id)) (scopeDecls e)
    where setScope = (buildInfo . biScope) %~ updateScope (scopeId e)
          setType = (buildInfo . scopeSetter e) ?~ scopeId e

  exitScope :: BuildContext m  => a -> m ()
  exitScope e = do
    clearScope
    modify (setType . setScope)
    where setScope = (buildInfo . biScope) %~ revertScope
          setType = (buildInfo . scopeSetter e) .~ Nothing

instance HasScope I.Module where
  scopeSetter _ = biModule
  scopeId I.Module{ I.moduleDecl } = moduleDecl

instance HasScope I.ImmutableContract where
  scopeSetter _ = biContract
  scopeId I.ImmutableContract{ I.contractName } = contractName
  scopeDecls I.ImmutableContract{ I.contractFields } = I.fieldName <$> contractFields

instance HasScope I.ProxyContract where
  scopeSetter _ = biProxy
  scopeId I.ProxyContract{ I.proxyName } = proxyName

instance HasScope I.FacetContract where
  scopeSetter _ = biFacet
  scopeId I.FacetContract{ I.facetName } = facetName
  enterScope e = do
    f <- getFacet (I.facetName e)
    modify (setType . setScope)
    traverse_ bringInScope (scopeDecls e)
    toggleProxyScope (f ^. facetProxy) True
    where setScope = (buildInfo . biScope) %~ updateScope (scopeId e)
          setType = (buildInfo . scopeSetter e) ?~ scopeId e
  exitScope e = do
    f <- getFacet (I.facetName e)
    clearScope
    modify (setType . setScope)
    toggleProxyScope (f ^. facetProxy) False
    where setScope = (buildInfo . biScope) %~ revertScope
          setType = (buildInfo . scopeSetter e) .~ Nothing

instance HasScope I.Function where
  scopeSetter _ = biFn
  scopeId I.Function{ I.functionHeader } = I.functionName functionHeader
  scopeDecls I.Function{ I.functionHeader } = I.argName <$>
    I.functionReturnType functionHeader : I.functionArgs functionHeader

updateScope :: Identifier -> Scope -> Scope
updateScope id s
  | T.null s = id
  | otherwise = s <> "::" <> id

revertScope :: Scope -> Scope
revertScope = T.intercalate "::" . Prelude.init . T.splitOn "::"

enterBlock :: BuildContext m => m ()
enterBlock = do
  modify $ (buildInfo . biDepth) +~ 1
  bd <- gets (^. (buildInfo . biDepth))
  modify $ (buildInfo . biScope) %~ updateScope (showT bd)

exitBlock :: BuildContext m => m ()
exitBlock = do
  modify $ (buildInfo . biDepth) -~ 1
  modify $ (buildInfo . biScope) %~ revertScope

bringInScope :: BuildContext m => I.Identifier -> m ()
bringInScope id = do
  s <- gets (^. (buildInfo . biScope))
  modify $ fields %~ M.adjust (\f -> f & fdInScope .~ True) (scopedId s)
  where scopedId s = s <> "::" <> id

toggleProxyScope :: BuildContext m => I.Identifier -> Bool -> m ()
toggleProxyScope pId flag = do
  p <- getProxy pId
  let fs = (\f -> p ^. proxyScope <> "::" <> p ^. proxyId <> "::" <> f) <$> (view _1 <$> p ^. proxyFields)
  traverse_ (\f -> modify $ fields %~ M.adjust (\f -> f & fdInScope .~ flag) f) fs

clearScope :: BuildContext m => m ()
clearScope = do
  s <- gets (^. (buildInfo . biScope))
  fsInScope <- gets (Prelude.filter (T.isPrefixOf s . dropField) . M.keys . (^. fields))
  traverse_ (\fd -> modify $ fields %~ M.adjust (\f -> f & fdInScope .~ False) fd) fsInScope
  where dropField = T.intercalate "::" . Prelude.init . T.splitOn "::"