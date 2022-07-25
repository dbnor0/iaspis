module Main where

import Prelude hiding (getLine)
import Data.Text.IO (getLine)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (decimal)

import Parser (uintRaw, bytesRaw)

main :: IO ()
main = do
    input <- getLine
    parseTest bytesRaw input
