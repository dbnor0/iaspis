module CLI.Parser where

import CLI.Commands.Compile
import CLI.Commands.Init
import Options.Applicative

commands :: Parser (IO ())
commands = subparser
  (  command "compile" (info (pure compileCmd) (progDesc "Compile Iaspis contracts to Solidity"))
  <> command "init" (info (pure initCmd) (progDesc "Initialize new Iaspis project in current directory"))
  )