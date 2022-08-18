module CLI.Parser where

import CLI.Types
import Options.Applicative
import Data.Semigroup ((<>))


buildCommand :: Parser BuildCommand
buildCommand = BuildCommand
  <$> strOption (long "path" <> metavar "CONTRACT_PATH" <> help "Path to contracts directory")
  <*> switch    (long "dumpAST" <> help "Flag for outputting parsed AST in JSON format")
