{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
module Transpile.Module where

import Iaspis.Grammar qualified as I
import Iaspis.DeclUtils
import Transpile.Storage
import Transpile.Types
import Transpile.Contract
import Transpile.Proxy
import Transpile.Facet
import Transpile.Struct (transpileStruct)
import Transpile.Enum (transpileEnum)

transpile :: [I.Module] -> [Module]
transpile ms =
  let cs = contractDecls =<< ms
      fs = facetDecls =<< ms
      ps = proxyDecls (snd <$> fs) =<< ms
      ss = structDecls =<< ms
      es = enumDecls =<< ms
      storageM = storageModule ps
  in 
    (transpileContract <$> cs) 
    <> (transpileProxy <$> ps) 
    <> (transpileFacet <$> fs)
    <> (transpileStruct <$> ss)
    <> (transpileEnum <$> es)
    <> [storageM]
