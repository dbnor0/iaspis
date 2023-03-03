-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE BlockArguments #-}

module Analysis.ContractCheck where

-- import Analysis.Environment.Environment
-- import Control.Monad.Error.Class
-- import Lens.Micro.Platform
-- import Data.Foldable
-- import Control.Monad
-- import Analysis.Environment.Error
-- import Control.Monad.State.Class
-- import Iaspis.Grammar hiding (facetList, proxyList)
-- import Data.Maybe
-- import Data.Map as M
-- import Analysis.Environment.Traversals
-- import Data.Text
-- import Analysis.Environment.Utils


-- definedProxies :: (MonadState BuildEnv m, MonadError BuildError m) => m ()
-- definedProxies = do
--   e <- gets (^. env)
--   traverse_ (\p -> traverse_ (checkProxy e p) (facetList p)) (e ^. proxies)
--   where checkProxy e p f = unless (f `Prelude.elem` (facetId <$> e ^. facets))
--           (throwError $ UndefFacet (proxyId p) f)

-- definedFacets :: MonadState BuildEnv m => MonadError BuildError m => m ()
-- definedFacets = do
--   e <- gets (^. env)
--   traverse_ (checkFacet e) (e ^. facets)
--   where checkFacet e f = unless (proxy f `Prelude.elem` (proxyId <$> e ^. proxies))
--           (throwError $ UndefProxy (facetId f) (proxy f))

-- contractCheck :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
-- contractCheck m@Module{ declarations } =
--   traverseModule m $ traverse_ contractCheckDecl declarations

-- contractCheckDecl :: MonadState BuildEnv m => MonadError BuildError m => Declaration -> m ()
-- contractCheckDecl = \case
--   ContractDecl c -> contractCheckContract c

-- contractCheckContract :: MonadState BuildEnv m => MonadError BuildError m => Contract -> m ()
-- contractCheckContract = traverseContract cFn pFn fFn
--   where cFn = const $ return ()
--         pFn = \case
--           ProxyContract _ _ facets fields -> do
--             traverse_ checkField fields
--             traverse_ checkFieldFacet fields
--             where checkField f = unless (isJust $ fieldProxyKind f)
--                     (throwError $ MissingProxyMemberKind (fieldName f))
--                   checkFieldFacet f =
--                     case fieldProxyKind f of
--                       Just (UniqueProxyMember facet) ->
--                         unless (facet `Prelude.elem` facets)
--                           (throwError $ UndeclaredFacet facet (fieldName f))
--                       _ -> return ()
--           _ -> return ()
--         fFn = \case
--           FacetContract fId _ fns -> do
--             ids <- validFacetFields fId
--             traverse_ (contractCheckFn ids) fns
--           _ -> return ()

-- contractCheckFn :: MonadState BuildEnv m => MonadError BuildError m => [Identifier] -> Function -> m ()
-- contractCheckFn ids f@Function{ functionBody } = traverseFn f $ traverse_ (contractCheckStmt ids) functionBody

-- contractCheckStmt :: MonadState BuildEnv m => MonadError BuildError m => [Identifier] -> Statement -> m ()
-- contractCheckStmt ids = \case
--   VarDeclStmt _ _ e -> contractCheckExpr ids e
--   AssignmentStmt id _ e -> contractCheckExpr ids id >> contractCheckExpr ids e
--   ReturnStmt e -> maybe (return ()) (contractCheckExpr ids) e
--   IfStmt cond b1 b2 -> do
--     contractCheckExpr ids cond
--     contractCheckStmt ids b1
--     maybe (return ()) (contractCheckStmt ids) b2
--   BlockStmt stmts -> traverse_ (contractCheckStmt ids) stmts
--   ExpressionStmt e -> contractCheckExpr ids e
--   _ -> return ()

-- contractCheckExpr :: MonadState BuildEnv m => MonadError BuildError m => [Identifier] -> Expression -> m ()
-- contractCheckExpr ids = \case
--   IdentifierE id -> contractCheckId ids id
--   FunctionCallE _ es -> traverse_ (contractCheckExpr ids) es
--   InstantiationE _ es -> traverse_ (contractCheckExpr ids) es
--   UnaryE _ e -> contractCheckExpr ids e
--   BinaryE _ e1 e2 -> contractCheckExpr ids e1 >> contractCheckExpr ids e2
--   _ -> return ()

-- contractCheckId :: MonadState BuildEnv m => MonadError BuildError m => [Identifier] -> Identifier -> m ()
-- contractCheckId ids id = do
--   (ct, _) <- getFacetField id
--   when (ct == Proxy) $ do
--     facet <- fromMaybe "" <$> gets (^. (scopeInfo . contract))
--     unless (id `Prelude.elem` ids) (throwError $ InvalidFieldRef id facet)

-- validFacetFields :: MonadState BuildEnv m => Identifier -> m [Identifier]
-- validFacetFields id = do
--     fieldEntries <- M.elems <$> gets (^. (env . varEntries))
--     proxies <- traverse fieldProxy fieldEntries
--     return $ fieldName . fst <$> Prelude.filter isProxyField (Prelude.zip (entry <$> fieldEntries) proxies)
--     where isProxyField (Field{ fieldProxyKind }, p) = case (fieldProxyKind, p) of
--             (Just (UniqueProxyMember fId), _) -> fId == id
--             (Just SharedProxyMember, Just p') -> id `Prelude.elem` facetList p'
--             _ -> False
--           fieldProxy (Entry s _) = getProxy (scopeContract s)
--           scopeContract = (!! 1) . splitOn "::"

-- checkContracts :: MonadState BuildEnv m => MonadError BuildError m => Module -> m ()
-- checkContracts m = do
--   definedFacets
--   definedProxies
--   contractCheck m

