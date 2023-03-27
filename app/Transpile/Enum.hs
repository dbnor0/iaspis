{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Transpile.Enum where

import Iaspis.Grammar qualified as I
import Solidity.Grammar qualified as S
import Transpile.Types qualified as T

transpileEnum :: I.Enum -> T.Module
transpileEnum I.Enum{ I.enumName, I.enumFields } = T.Module
  { T.imports = []
  , T.moduleId = enumName
  , T.decls = [T.EnumDef $ S.EnumDefinition enumName enumFields]
  }