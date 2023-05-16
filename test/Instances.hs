module Instances where

import Transpiler.Iaspis.Grammar

instance Arbitrary MemoryLocation where
  arbitrary = oneof [pure Storage, pure Memory]