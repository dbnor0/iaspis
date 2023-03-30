{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen.Types where

import Lens.Micro.Platform
import Data.Text as T
import Control.Monad.State


type SolText = T.Text

newtype GenState = GenState
  { _indentation :: Int
  } deriving stock (Eq, Show)

makeLenses ''GenState

indent :: Int -> T.Text
indent = flip T.replicate "  "

genText :: SolText -> State GenState SolText
genText t = do
  ind <- gets (^. indentation)
  return $ indent ind <> t
