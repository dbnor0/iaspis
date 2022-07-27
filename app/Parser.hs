{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Functor (($>))
import Data.Text
import Data.Void
import Data.Word
import Text.Megaparsec
import Data.Char (isDigit, isHexDigit, isAlphaNum, isAlpha)
import Text.Megaparsec.Char.Lexer (decimal, hexadecimal, skipLineComment, skipBlockComment, space, lexeme, charLiteral)
import Numeric (showHex)
import Prelude hiding (length, lex, Enum)
import Control.Monad (guard, void)
import Text.Megaparsec.Char (char, space1)
import Value (Value (..))
import Source

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = skipLineComment "//"

blockComment :: Parser ()
blockComment = skipBlockComment "/*" "*/"

whitespace :: Parser ()
whitespace = space1

spaceOrComment :: Parser ()
spaceOrComment = space whitespace lineComment blockComment

source' :: Parser Source
source' = Source <$> many (try importStmt) <*> many decl 

source :: Parser Source
source = source' <* eof

semicolon :: Parser ()
semicolon = reserved ";"

comma :: Parser ()
comma = reserved ","

reserved :: Text -> Parser ()
reserved = void . lex . chunk

block :: Parser a -> Parser a
block = between (reserved "{") (reserved "}")

importStmt :: Parser Import
importStmt = Import <$> (reserved "import" *> stringRaw <* semicolon)

decl :: Parser Declaration
decl = choice $ try <$> [StructDecl <$> struct, EnumDecl <$> enum, ContractDecl <$> contract]

struct :: Parser Struct
struct = Struct <$> (reserved "struct" *> identifier <* members)
    where members = block (lex "struct")

enum :: Parser Enum
enum = Enum <$> (reserved "enum" *> identifier <* members)
    where members = block (lex "enum")

contract :: Parser Contract
contract = choice $ try <$> [immutableContract, proxyContract, facetContract]

immutableContract :: Parser Contract
immutableContract = ImmutableContract <$> name <*> body
    where name = reserved "contract" *> identifier
          body = block members
          members = sepBy (lex "member") semicolon 

proxyContract :: Parser Contract
proxyContract = ProxyContract <$> kind' <*> name  <*> facetList
    where kind' = kind <* reserved "proxy"
          name = identifier <* reserved "for"
          facetList = sepBy identifier comma

kind :: Parser ProxyKind
kind = ProxyOpen <$ reserved "open" <|> ProxyClosed <$ reserved "closed"

facetContract :: Parser Contract
facetContract = FacetContract <$> name <*> proxyList
    where name = reserved "facet" *> identifier
          proxyList = sepBy identifier comma

identifier :: Parser Text
identifier = lex $ cons <$> satisfy isAlpha <*> takeWhile1P Nothing isAlphaNum

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