module CLI.Parser where

import CLI.Command
import Options.Applicative

commands :: Parser (IO ())
commands = subparser
  ( command "compile" (info (pure compileCmd) (progDesc "Compile Iaspis contracts to Solidity"))
  )