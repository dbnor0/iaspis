{-# LANGUAGE OverloadedStrings #-}

module Main where

import CLI.Commands (build)
import CLI.Parser (buildCommand)
import Options.Applicative
import Parser.Source (module')
import System.Directory (getDirectoryContents, getCurrentDirectory, doesDirectoryExist)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)


main :: IO ()
main = do
  build =<< execParser opts
  where opts = info (buildCommand <**> helper) (fullDesc <> progDesc "Iaspis compiler")
