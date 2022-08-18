{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Commands where

import           CLI.Types
import           CLI.Util
import qualified Data.Text.IO as T
import           Text.Megaparsec (runParser)
import           Parser.Source (module')


build :: BuildCommand -> IO ()
build BuildCommand { path, .. } = do
  contracts <- getContractFiles path
  contents <- traverse T.readFile contracts
  print $ runParser module' "" <$> contents
