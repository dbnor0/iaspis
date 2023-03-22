{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
module Transpile.Module where

import Iaspis.Grammar qualified as I
import Iaspis.DeclUtils
import Data.Maybe
import Transpile.Storage
import Transpile.Types
import Transpile.Contract
import Transpile.Proxy
import Transpile.Facet

transpile :: [I.Module] -> [Module]
transpile ms =
  let cs = contractDecls =<< ms
      fs = facetDecls =<< ms
      ps = proxyDecls (snd <$> fs) =<< ms
      storageM = Just $ storageModule ps
  in catMaybes $ (transpileContract <$> cs) <> (transpileProxy <$> ps) <> (transpileFacet <$> fs) <> [storageM]
