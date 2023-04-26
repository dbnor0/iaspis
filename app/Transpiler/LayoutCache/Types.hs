{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}

module Transpiler.LayoutCache.Types where
  
import Data.Aeson
import GHC.Generics
import Data.Text qualified as T

type Id = T.Text
type Type = T.Text

data LayoutDecl = LayoutDecl Id Type
  deriving stock (Eq, Show, Generic)

instance ToJSON LayoutDecl where
instance FromJSON LayoutDecl where

data MemoryLayout = MemoryLayout Id [LayoutDecl]
   deriving stock (Eq, Show, Generic)

instance ToJSON MemoryLayout where
instance FromJSON MemoryLayout where
