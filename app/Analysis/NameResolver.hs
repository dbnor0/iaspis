{-# LANGUAGE DerivingStrategies #-}
module Analysis.NameResolver where

import Iaspis.Source
import Iaspis.Utils (declId)
import Analysis.Utils (unique)


data ResolverError
  = DupContracts
  deriving stock (Eq, Show)

uniqueContracts :: Module -> Either ResolverError ()
uniqueContracts m
  | contractsUnique m = Right ()
  | otherwise         = Left DupContracts
  where contractsUnique = unique . fmap declId . declarations