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
decl = choice $ try <$> [struct, enum, contract]

struct :: Parser Declaration
struct = StructDecl <$> (Struct <$> name <*> fields)
    where name      = reserved "struct" *> identifier
          fields    = block $ many fieldDecl
          fieldDecl = field <* semicolon
          field     = (,) <$> type' <*> identifier 

enum :: Parser Declaration
enum = EnumDecl <$> (Enum <$> name <*> fields)
    where name   = reserved "enum" *> identifier
          fields = block $ sepBy1 identifier comma

contract :: Parser Declaration
contract = ContractDecl <$> choice (try <$> [immutableContract, proxyContract, facetContract])

immutableContract :: Parser Contract
immutableContract = ImmutableContract <$> name <*> inheritanceList <*> memberList
    where name            = reserved "contract" *> identifier
          inheritanceList = sepBy identifier comma

proxyContract :: Parser Contract
proxyContract = ProxyContract <$> kind' <*> name  <*> facetList <*> memberList
    where kind'      = kind <* reserved "proxy"
          name       = identifier <* reserved "for"
          facetList  = sepBy identifier comma

kind :: Parser ProxyKind
kind = ProxyOpen <$ reserved "open" <|> ProxyClosed <$ reserved "closed"

facetContract :: Parser Contract
facetContract = FacetContract <$> name <*> proxyList <*> memberList
    where name       = reserved "facet" *> identifier
          proxyList  = reserved "to" *> sepBy identifier comma

memberList :: Parser [MemberDecl]
memberList = block $ many memberDecl

memberDecl :: Parser MemberDecl
memberDecl = choice $ try <$> [fieldDecl, functionDecl]

fieldDecl :: Parser MemberDecl
fieldDecl = field <* semicolon
    where field      = FieldDecl <$> proxyKind <*> visibility <*> modifiers <*> type' <*> identifier  
          modifiers  = many modifier 

proxyKind :: Parser ProxyMemberKind
proxyKind = reserved "@" *> facetId
    where facetId = choice $ try <$> [SharedProxyMember <$ reserved "*", UniqueProxyMember <$> identifier]

visibility :: Parser MemberVisibility
visibility = choice $ try <$> 
    [ Public <$ reserved "pub"
    , Private <$ reserved "pvt"
    , Internal <$ reserved "int"
    , External <$ reserved "ext"
    ]

modifier :: Parser FieldModifier
modifier = choice $ try <$> [ConstMod <$ reserved "const"]

functionDecl :: Parser MemberDecl
functionDecl = FunctionDecl <$> visibility <*> payability <*> functionKind' <*> identifier <*> argList <* body
    where body = block $ reserved "body" 

payability :: Parser PayabilityKind
payability = choice $ try <$> [Payable <$ reserved "$", NonPayable <$ reserved "!"]

functionKind' :: Parser FunctionKind
functionKind' = choice $ try <$> [Function <$ reserved "fn", Procedure <$ reserved "proc"]

argList :: Parser [(Type, Identifier)]
argList = between (reserved "(") (reserved ")") args
    where args = sepBy arg comma
          arg  = (,) <$> type' <*> identifier

identifier :: Parser Text
identifier = lex $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum

type' :: Parser Type
type' = choice $ try <$> 
    [ AddressT <$ reserved "address"
    , BoolT <$ reserved "bool"
    , DynamicBytesT <$ reserved "bytes"
    , StringT <$ reserved "string"
    ]

lex :: Parser a -> Parser a
lex = lexeme spaceOrComment

-- TODO: workaround for pack
stringRaw :: Parser Text
stringRaw = pack <$> (char '\"' *> manyTill charLiteral (char '\"'))


-- uintRaw :: Parser Word
-- uintRaw = lex decimal

-- boolRaw :: Parser Bool
-- boolRaw = lex $ True <$ chunk "true" <|> False <$ chunk "false"

-- hexRaw :: Parser Text
-- hexRaw = takeWhile1P Nothing isHexDigit

-- bytesRaw :: Parser Text
-- bytesRaw = lex hexRaw

-- addressLit :: Parser Value
-- addressLit = AddressV <$> bytesRaw

-- boolLit :: Parser Value
-- boolLit = BoolV <$> boolRaw

-- bytesLit :: Parser Value
-- bytesLit = BytesV <$> bytesRaw

-- stringLit :: Parser Value
-- stringLit = StringV <$> stringRaw

-- uintLit :: Parser Value
-- uintLit = UIntV <$> uintRaw


literal :: Parser Value
literal = undefined