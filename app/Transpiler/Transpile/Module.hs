{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Transpiler.Transpile.Module where

import Transpiler.Iaspis.Grammar qualified as I
import Transpiler.Iaspis.DeclUtils
import Transpiler.Transpile.Storage
import Transpiler.Transpile.Types
import Transpiler.Transpile.Contract
import Transpiler.Transpile.Proxy
import Transpiler.Transpile.Facet
import Transpiler.Transpile.Struct (transpileStruct)
import Transpiler.Transpile.Enum (transpileEnum)
import Lens.Micro.Platform
import Transpiler.Analysis.Environment

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
