{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Analysis.Environment.Build where

import Analysis.Environment.Environment
import Control.Monad.State.Class
import Data.Map as M
import Data.Foldable (traverse_)
import Lens.Micro.Platform
import Control.Monad.Error.Class ( MonadError(throwError) )
import Control.Monad
import Iaspis.Grammar
import Analysis.Environment.Error
import Utils.Text ( showT )
import Data.Maybe
import Analysis.Environment.Traversals
import Analysis.Environment.Utils


buildEnv :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
buildEnv m@Module{ moduleDecl, imports, declarations } = traverseModule m $ do
  e <- gets (^. env)
  uniqueId moduleDecl (moduleId <$> e ^. modules) DupModule
  modify (& (env . modules) %~ (ModuleEntry moduleDecl imports :))
  traverse_ addDecls declarations

addDecls :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
addDecls = \case
  ContractDecl c -> addContract c

addContract :: MonadState BuildEnv m => MonadError BuildError m => Contract -> m ()
addContract = traverseContract cFn pFn fFn
  where cFn = \case
          ImmutableContract cId cFields cFns -> do
            e <- gets (^. env)
            s <- gets (^. (scopeInfo . module'))
            uniqueId cId (contractId <$> e ^. contracts) DupContract
            modify (& (env . typeEntries) %~ M.insert (s <> "::" <> cId) (Entry s (UserDefinedT cId)))
            modify (& (env . contracts) %~ (ContractEntry cId s :))
            traverse_ addField cFields >> traverse_ addFn cFns
          _ -> return ()
        pFn = \case
          ProxyContract _ pId facetList pFields -> do
            e <- gets (^. env)
            s <- gets (^. (scopeInfo . module'))
            uniqueId pId (proxyId <$> e ^. proxies) DupProxy
            modify (& (env . proxies) %~ (ProxyEntry pId s facetList :))
            traverse_ addField pFields
          _ -> return ()
        fFn = \case
          FacetContract fId proxyList fFns -> do
            e <- gets (^. env)
            s <- gets (^. (scopeInfo . module'))
            uniqueId fId (facetId <$> e ^. facets) DupFacet
            modify (& (env . facets) %~ (FacetEntry fId s proxyList :))
            traverse_ addFn fFns
          _ -> return ()

addField :: MonadState BuildEnv m => MonadError BuildError m => Field -> m ()
addField f@Field{ fieldName } = do
  s <- gets (^. scope)
  e <- gets (^. env)
  let scopedName = s <> "::" <> fieldName
      field = Entry s f
  uniqueId scopedName (M.keys $ currentScope s (e ^. varEntries)) (DupField s)
  modify (& (env . varEntries) %~ M.insert scopedName field)

currentScope :: Scope -> Bindings a -> Bindings a
currentScope s = M.filter ((== s) . entryScope)

addFn :: MonadState BuildEnv m => MonadError BuildError m => Function -> m ()
addFn f@Function{ functionHeader, functionBody } = 
  traverseFn f $ do
    m <- gets (^. (scopeInfo . module')) 
    c <- gets (^. (scopeInfo . contract))
    e <- gets (^. env)
    let s = m <> "::" <> fromMaybe "" c
    uniqueId (s <> "::" <> fnName) (M.keys $ currentScope s (e ^. fnEntries)) (DupFn s)
    modify (& (env . fnEntries) %~ M.insert (s <> "::" <> fnName) (Entry s functionHeader))
    traverse_ addField fnArgs >> traverse_ addStmt functionBody
    where fnName = functionName functionHeader
          fnArgs = functionArgs functionHeader

addStmt :: MonadState BuildEnv m => MonadError BuildError m => Statement -> m ()
addStmt =
  \case
    VarDeclStmt f _ _ -> addField f
    BlockStmt stmts -> do
      bs <- gets (showT . ( ^. blockDepth))
      withScope bs $ traverse_ addStmt stmts
      return ()
    _ -> return ()

uniqueId :: MonadError BuildError m => Identifier -> [Identifier] -> (Identifier -> BuildError) -> m ()
uniqueId id env errC = when (id `elem` env) (throwError $ errC id)

