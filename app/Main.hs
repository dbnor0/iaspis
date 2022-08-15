{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude hiding (getLine, readFile)
import Data.Text.IO (getLine, readFile)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)

import Parser (source)
import System.Directory (getDirectoryContents, getCurrentDirectory, doesDirectoryExist)
import ModuleLoader (getContractFiles)
import Options.Applicative
import CommandLine (BuildCommand (BuildCommand, path), buildCommand)

build :: BuildCommand -> IO ()
build BuildCommand { path, .. } = do
    contracts <- getContractFiles path
    contents <- traverse readFile contracts
    print $ runParser source "" <$> contents

main :: IO ()
main = do
    build =<< execParser opts
    where opts = info (buildCommand <**> helper)
            (fullDesc
            <> progDesc "Compiler for a general purpose EVM compatible blockchain programming language"
            )