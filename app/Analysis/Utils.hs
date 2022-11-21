module Analysis.Utils where

import Data.Containers.ListUtils

unique :: (Ord a) =>[a] -> Bool
unique xs = xs == nubOrd xs