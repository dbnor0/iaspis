{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Functor (($>))
import Data.Text
import Data.Void
import Data.Word
import Text.Megaparsec
import Data.Char (isDigit, isHexDigit)
import Text.Megaparsec.Char.Lexer (decimal, hexadecimal, skipLineComment, skipBlockComment, space, lexeme, charLiteral)
import Numeric (showHex)
import Prelude hiding (length, lex)
import Control.Monad (guard)
import Text.Megaparsec.Char (char, space1)
import Value (Value (..))

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = skipLineComment "//"

blockComment :: Parser ()
blockComment = skipBlockComment "/*" "*/"

whitespace :: Parser ()
whitespace = space1

spaceOrComment :: Parser ()
spaceOrComment = space whitespace lineComment blockComment

lex :: Parser a -> Parser a
lex = lexeme spaceOrComment

uintRaw :: Parser Word
uintRaw = lex decimal

boolRaw :: Parser Bool
boolRaw = lex $ True <$ chunk "true" <|> False <$ chunk "false"

hexRaw :: Parser Text
hexRaw = takeWhile1P Nothing isHexDigit 

bytesRaw :: Parser Text
bytesRaw = lex hexRaw

-- TODO: workaround for pack
stringRaw :: Parser Text
stringRaw = pack <$> (char '\"' *> manyTill charLiteral (char '\"'))

addressLit :: Parser Value
addressLit = AddressV <$> bytesRaw

boolLit :: Parser Value
boolLit = BoolV <$> boolRaw

bytesLit :: Parser Value
bytesLit = BytesV <$> bytesRaw

stringLit :: Parser Value
stringLit = StringV <$> stringRaw

uintLit :: Parser Value
uintLit = UIntV <$> uintRaw

{-
    literal
      

-}

literal :: Parser Value
literal = undefined