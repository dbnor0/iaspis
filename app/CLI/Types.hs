module CLI.Types where


data BuildCommand = BuildCommand
  { path :: String
  , dumpAst :: Bool
  } deriving (Eq, Show)
