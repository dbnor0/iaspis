{-# LANGUAGE DerivingStrategies #-}

module CLI.Commands.Compile where

import Transpiler.Entry
import CLI.Utils (inProjectRoot, runShell)
import System.Directory
import Control.Monad

compileCmd :: IO ()
compileCmd = do
  inProjectRoot
  entry
  compileSolidity

compileSolidity :: IO ()
compileSolidity = withCurrentDirectory "./scripts" $ do
  putStrLn "Compiling Solidity..."
  void $ runShell "truffle compile" solidityMsg
  putStrLn "Compiled Solidity"

solidityMsg :: String
solidityMsg = "Encountered errors compiling to Solidity"