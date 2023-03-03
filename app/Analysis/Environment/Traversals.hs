-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Analysis.Environment.Traversals where

-- import Control.Monad.State.Class
-- import Iaspis.Grammar
-- import Lens.Micro.Platform
-- import Analysis.Environment.Environment
-- import Analysis.Environment.Utils


-- traverseModule :: MonadState BuildEnv m => Module -> m a -> m ()
-- traverseModule Module{ moduleDecl } f = do
--   modify (& (scopeInfo . module') .~ moduleDecl)
--   withScope moduleDecl f

-- traverseContract :: MonadState BuildEnv m => (Contract -> m a) -> (Contract -> m a) -> (Contract -> m a) -> Contract -> m ()
-- traverseContract cFn pFn fFn m =
--   case m of
--     ImmutableContract cId _ _ -> do
--       modify (& (scopeInfo . contract) ?~ cId)
--       modify (& (scopeInfo . contractType) ?~ Immutable)
--       withScope cId (cFn m)
--     ProxyContract _ pId _ _ -> do
--       modify (& (scopeInfo . contract) ?~ pId)
--       modify (& (scopeInfo . contractType) ?~ Proxy)
--       withScope pId (pFn m)
--     FacetContract fId _ _ -> do
--       modify (& (scopeInfo . contract) ?~ fId)
--       modify (& (scopeInfo . contractType) ?~ Facet)
--       withScope fId (fFn m)

-- traverseFn :: MonadState BuildEnv m => Function -> m a -> m ()
-- traverseFn Function{ functionHeader } f= do
--   modify (& (scopeInfo . fn) ?~ fnName)
--   withScope fnName f
--   where fnName = functionName functionHeader