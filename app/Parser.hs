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
import Control.Monad (guard, void, when)
import Text.Megaparsec.Char (char, space1)
import Source
import Data.Functor.Contravariant (Predicate (getPredicate, Predicate))
import Utils (uintPredicates, testPredicates, bytesPredicates)

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = skipLineComment "//"

blockComment :: Parser ()
blockComment = skipBlockComment "/*" "*/"

whitespace :: Parser ()
whitespace = space1

spaceOrComment :: Parser ()
spaceOrComment = space whitespace lineComment blockComment

reserved :: Text -> Parser ()
reserved = void . lex . chunk

block :: Parser a -> Parser a
block = between (reserved "{") (reserved "}")

backtrack :: [Parser a] -> Parser a
backtrack = choice . (<$>) try

source' :: Parser Source
source' = Source <$> many (try importStmt) <*> many decl

source :: Parser Source
source = source' <* eof

semicolon :: Parser ()
semicolon = reserved ";"

comma :: Parser ()
comma = reserved ","

endsIn :: Text -> Parser a -> Parser a
endsIn t = flip (<*) (reserved t)

keyword :: Text -> a -> Parser a
keyword k = (<$ reserved k)

importStmt :: Parser Import
importStmt = Import <$> endsIn ";" import'
    where import' = reserved "import" *> stringRaw

decl :: Parser Declaration
decl = backtrack [struct, enum, contract]

struct :: Parser Declaration
struct = StructDecl <$> (Struct <$> name <*> fields)
    where name      = reserved "struct" *> identifier
          fields    = block $ many fieldDecl
          fieldDecl = endsIn ";" arg

enum :: Parser Declaration
enum = EnumDecl <$> (Enum <$> name <*> fields)
    where name   = reserved "enum" *> identifier
          fields = block $ sepBy1 identifier comma

contract :: Parser Declaration
contract = ContractDecl <$> backtrack [immutableContract, proxyContract, facetContract]

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
memberDecl = backtrack [fieldDecl, functionDecl]


fieldDecl :: Parser MemberDecl
fieldDecl = endsIn ";" field
    where field      = FieldDecl <$> proxyKind <*> visibility <*> modifiers <*> type' <*> identifier
          modifiers  = many modifier

proxyKind :: Parser ProxyMemberKind
proxyKind = reserved "@" *> backtrack
    [ keyword "*" SharedProxyMember
    , UniqueProxyMember <$> identifier
    ]

visibility :: Parser MemberVisibility
visibility = backtrack
    [ keyword "pub" Public
    , keyword "pvt" Private
    , keyword "int" Internal
    , keyword "ext" External
    ]

modifier :: Parser FieldModifier
modifier = backtrack [keyword "const" ConstMod]

functionDecl :: Parser MemberDecl
functionDecl = FunctionDecl <$> visibility <*> payability <*> functionKind' <*> identifier <*> functionSig' <*> body
    where body = many statement

payability :: Parser PayabilityKind
payability = backtrack
    [ keyword "$" Payable
    , keyword "!" NonPayable
    ]

functionKind' :: Parser FunctionKind
functionKind' = backtrack
    [ keyword "fn" Function
    , keyword "proc" Procedure
    ]

functionSig' :: Parser FunctionSignature
functionSig' = FunctionSignature <$> argList <*> returnType
    where argList = between (reserved "(") (reserved ")") (sepBy arg comma)
          returnType = reserved "=>" *> type'

arg :: Parser Arg
arg = Arg <$> type' <*> optional memoryLocation  <*> identifier

memoryLocation :: Parser MemoryLocation
memoryLocation = backtrack
    [ keyword "storage" Storage
    , keyword "memory" Memory
    ]

identifier :: Parser Text
identifier = lex $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum

sizedType :: (Int -> Type) -> [Int -> Bool] -> Parser Type
sizedType c ps = c <$> typeSize ps

type' :: Parser Type
type' = backtrack [arrayType, mappingType, primitiveType]

primitiveType :: Parser Type
primitiveType =  backtrack
    [ keyword "address" AddressT
    , keyword "bool" BoolT
    , keyword "string" StringT
    , chunk "uint" *> sizedType UIntT uintPredicates
    , chunk "bytes" *> sizedType BytesT bytesPredicates
    , keyword "bytes" BytesDynamicT
    ]

arrayType :: Parser Type
arrayType = ArrayT <$> primitiveType <*> dimensions
    where dimensions = many $ reserved "[" *> optional uintRaw <* reserved "]"

mappingType :: Parser Type
mappingType = MappingT <$> key <*> value
    where key   = reserved "mapping" *> reserved "(" *> type' <* reserved "=>"
          value = type' <* reserved ")"

typeSize :: [Int -> Bool] -> Parser Int
typeSize ps = do
    size <- uintRaw
    guard $ testPredicates size ps
    return size

statement :: Parser Statement
statement = backtrack
    [ varDeclStmt
    , returnStmt
    , assignmentStmt
    , blockStmt
    , ifStmt
    ]

varDeclStmt :: Parser Statement
varDeclStmt = endsIn ";" stmt
    where stmt = VarDeclStmt <$> arg <*> optional ((,) <$> assignmentSymbol <*> expression)

returnStmt :: Parser Statement
returnStmt = endsIn ";" stmt
    where stmt = ReturnStmt <$> (reserved "return" *> expression)

assignmentStmt :: Parser Statement
assignmentStmt = endsIn ";" stmt
    where stmt = AssignmentStmt <$> identifier <*> assignmentSymbol <*> expression

assignmentSymbol :: Parser MemoryLocation
assignmentSymbol = reserved "<-" $> Storage <|> reserved ":=" $> Memory

blockStmt :: Parser Statement
blockStmt = BlockStmt <$> block (many statement)

ifStmt :: Parser Statement
ifStmt = IfStmt <$> cond <*> ifBranch <*> elseBranch
    where cond = reserved "if" *> between (reserved "(") (reserved ")") expression
          ifBranch = statement
          elseBranch = optional $ reserved "else" *> statement

expression :: Parser Expression
expression = backtrack [functionCallExpr, literalExpr, identifierExpr, binaryExpr]

-- precedence

binaryExpr :: Parser Expression
binaryExpr = BinaryE <$> factor <*> op <*> expression
    where factor = literalExpr <|> identifierExpr
          op     = reserved "+" $> AdditionOp <|> reserved "-" $> SubtractionOp

literalExpr :: Parser Expression
literalExpr = LiteralE <$> literal

identifierExpr :: Parser Expression
identifierExpr = IdentifierE <$> identifier

functionCallExpr :: Parser Expression
functionCallExpr = FunctionCallE <$> identifier <*> argList
    where argList = between (reserved "(") (reserved ")") args
          args = sepBy expression (reserved ",")

lex :: Parser a -> Parser a
lex = lexeme spaceOrComment

-- TODO: workaround for pack
stringRaw :: Parser Text
stringRaw = pack <$> (char '\"' *> manyTill charLiteral (char '\"'))

uintRaw :: Parser Int
uintRaw = lex decimal

boolRaw :: Parser Bool
boolRaw = lex $ True <$ chunk "true" <|> False <$ chunk "false"

hexRaw :: Parser Text
hexRaw = chunk "0x" *> takeWhile1P Nothing isHexDigit

bytesRaw :: Parser Text
bytesRaw = lex hexRaw

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

literal :: Parser Value
literal = backtrack
    [ addressLit
    , boolLit
    , bytesLit
    , stringLit
    , uintLit
    ]