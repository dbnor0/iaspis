module Main where

import Transpiler.Iaspis.Grammar
import Test.QuickCheck

instance Arbitrary MemoryLocation where
  arbitrary = oneof [pure Storage, pure Memory]

main :: IO ()
main = do
  quickCheck prop_memoryLocationEquality

prop_memoryLocationEquality :: MemoryLocation -> MemoryLocation -> Property
prop_memoryLocationEquality l1 l2 = (l1 == l2) === (l2 == l1)