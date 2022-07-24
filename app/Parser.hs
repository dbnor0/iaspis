{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Functor (($>))
import Data.Text
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte (digitChar)
import Data.Char (isDigit, isHexDigit)
import Text.Megaparsec.Char.Lexer (decimal, hexadecimal)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (many1)
import Prelude hiding (length)
import Control.Monad (guard)

type Parser = Parsec Void Text

showHex' :: (Integral a, Show a) => a -> Text
showHex' = pack . flip showHex ""

uintRaw :: Parser Word
uintRaw = decimal

boolRaw :: Parser Bool
boolRaw = True <$ chunk "true" <|> False <$ chunk "false"

hexRaw :: Parser Text
hexRaw = takeWhile1P Nothing isHexDigit 

addressRaw :: Parser Text
addressRaw = do
    hex <- chunk "0x" *> hexRaw
    guard $ length hex == 20
    return hex