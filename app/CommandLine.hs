module CommandLine where

import Options.Applicative
import Data.Semigroup ((<>))

data BuildCommand = BuildCommand
    { path :: String
    , dumpAst :: Bool
    } deriving (Eq, Show)

buildCommand :: Parser BuildCommand
buildCommand = BuildCommand
    <$> strOption (long "path" <> metavar "CONTRACT_PATH" <> help "Path to contracts directory")
    <*> switch (long "dumpAST" <> help "Flag for outputting parsed AST in JSON format")