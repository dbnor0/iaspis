{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (getLine, readFile)
import Data.Text.IO (getLine, readFile)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)

import Parser (source)

main :: IO ()
main = do
    input <- readFile "./contracts/HelloWorld.ip"
    case parse source "" input of
        Left err -> print err
        Right ast -> print ast
