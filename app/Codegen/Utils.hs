{-# LANGUAGE OverloadedStrings #-}

module Codegen.Utils where

import  Data.Text as T
import Codegen.Types
import Control.Monad.State
import Lens.Micro.Platform

indent :: Int -> T.Text
indent = flip T.replicate "  "

genText :: SolText -> State GenState SolText
genText t = do
  ind <- gets (^. indentation)
  return $ indent ind <> t