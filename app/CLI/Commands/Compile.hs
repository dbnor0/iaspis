{-# LANGUAGE DerivingStrategies #-}

module CLI.Commands.Compile where

import Transpiler.Entry
import CLI.Utils (inProjectRoot)

compileCmd :: IO ()
compileCmd = do
  inProjectRoot
  entry