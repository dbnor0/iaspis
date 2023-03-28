{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Transpile.Enum where

import Iaspis.Grammar qualified as I
import Solidity.Grammar qualified as S
import Transpile.Types qualified as T
import Analysis.Environment

transpileEnum :: BuildContext m => I.Enum -> m T.Module
transpileEnum I.Enum{ I.enumName, I.enumFields } = 
  return T.Module
    { T.imports = []
    , T.moduleId = enumName
    , T.decls = [T.EnumDef $ S.EnumDefinition enumName enumFields]
    }