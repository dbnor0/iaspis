{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Lens.Micro.Platform
import Analysis.Environment

transpile :: BuildContext m => [I.Module] -> m [Module]
transpile ms = do
  tcs <- traverse transpileContract cs
  tps <- traverse transpileProxy ps
  tfs <- traverse transpileFacet fs
  tss <- traverse transpileStruct ss
  tes <- traverse transpileEnum es
  return $ storageM : tcs <> tps <> tfs <> tss <> tes
  where cs = contractDecls =<< ms
        fs = facetDecls =<< ms
        ps = proxyDecls (view _3 <$> fs) =<< ms
        ss = structDecls =<< ms
        es = enumDecls =<< ms
        storageM = storageModule ps
