module CLI.Parser where

import CLI.Commands.Compile
import CLI.Commands.Init
import Options.Applicative
import CLI.Commands.Account
import CLI.Commands.Clear

commands :: Parser (IO ())
commands = subparser
  (  command "compile" (info (pure compileCmd) (progDesc "Compile Iaspis contracts to Solidity"))
  <> command "init" (info (pure initCmd) (progDesc "Initialize new Iaspis project in current directory"))
  <> command "account" (info (pure accountCmd) (progDesc "Open up Truffle config file for account & network info"))
  <> command "clear" (info (pure clearCmd) (progDesc "Clear all cached memory layout files"))
  )