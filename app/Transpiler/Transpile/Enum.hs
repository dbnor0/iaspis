{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Transpiler.Transpile.Enum where

import Transpiler.Iaspis.Grammar qualified as I
import Transpiler.Solidity.Grammar qualified as S
import Transpiler.Transpile.Types qualified as T
import Transpiler.Analysis.Environment

transpileEnum :: BuildContext m => I.Enum -> m T.Module
transpileEnum I.Enum{ I.enumName, I.enumFields } = 
  return T.Module
    { T.imports = []
    , T.moduleId = enumName
    , T.decls = [T.EnumDef $ S.EnumDefinition enumName enumFields]
    }