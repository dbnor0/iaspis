{-# LANGUAGE DerivingStrategies #-}

module CLI.Command where

import Transpiler.Entry

compileCmd :: IO ()
compileCmd = entry
