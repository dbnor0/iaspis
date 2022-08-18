{-# LANGUAGE OverloadedStrings #-}

module Parser.Source where

import Control.Monad (guard)
import Data.Char (isHexDigit, isAlpha, isAlphaNum)
import Data.Text
import Parser.Base
import Parser.Types 
import Parser.Util
import Iaspis.Source hiding (moduleDecl, fieldProxyKind, functionKind)
import Text.Megaparsec 
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, charLiteral)


module' :: Parser Module
module' = module'' <* eof
  where module'' = Module <$> moduleDecl <*> many (try importStmt) <*> many decl

-- top level elements

moduleDecl :: Parser ModuleDecl
moduleDecl = ModuleDecl <$> endsIn ";" (reserved "module" *> identifier)

importStmt :: Parser Import
importStmt = Import <$> endsIn ";" import'
  where import' = reserved "import" *> identifier

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
proxyContract = ProxyContract <$> proxyKind' <*> name  <*> facetList <*> memberList
  where proxyKind' = proxyKind <* reserved "proxy"
        name       = identifier <* reserved "for"
        facetList  = sepBy identifier comma

facetContract :: Parser Contract
facetContract = FacetContract <$> name <*> proxyList <*> memberList
  where name       = reserved "facet" *> identifier
        proxyList  = reserved "to" *> sepBy identifier comma

-- field declarations

memberList :: Parser [MemberDecl]
memberList = block $ many memberDecl

memberDecl :: Parser MemberDecl
memberDecl = backtrack [fieldDecl, functionDecl]

fieldDecl :: Parser MemberDecl
fieldDecl = endsIn ";" field
  where field     = FieldDecl <$> optional fieldProxyKind <*> optional visibility <*> modifiers <*> type' <*> identifier
        modifiers = many modifier

forFieldDecl :: Parser MemberDecl
forFieldDecl = FieldDecl Nothing Nothing [] <$> type' <*> identifier

functionDecl :: Parser MemberDecl
functionDecl = FunctionDecl <$> visibility <*> payability <*> functionKind <*> identifier <*> functionSig' <*> body
  where body = many statement

-- syntax elements

proxyKind :: Parser ProxyKind
proxyKind = ProxyOpen <$ reserved "open" <|> ProxyClosed <$ reserved "closed"

fieldProxyKind :: Parser ProxyMemberKind
fieldProxyKind = reserved "@" *> backtrack
  [ reserved' "*" SharedProxyMember
  , UniqueProxyMember <$> identifier
  ]

visibility :: Parser MemberVisibility
visibility = backtrack
  [ reserved' "pub" Public
  , reserved' "pvt" Private
  , reserved' "int" Internal
  , reserved' "ext" External
  ]

modifier :: Parser FieldModifier
modifier = backtrack [reserved' "const" ConstMod]

payability :: Parser PayabilityKind
payability = backtrack
  [ reserved' "$" Payable
  , reserved' "!" NonPayable
  ]

functionKind :: Parser FunctionKind
functionKind = backtrack
  [ reserved' "fn" Function
  , reserved' "proc" Procedure
  ]

functionSig' :: Parser FunctionSignature
functionSig' = FunctionSignature <$> argList <*> returnType
  where argList    = parens (sepBy arg comma)
        returnType = reserved "=>" *> type'

memoryLocation :: Parser MemoryLocation
memoryLocation = backtrack
  [ reserved' "storage" Storage
  , reserved' "memory" Memory
  ]

arg :: Parser Arg
arg = Arg <$> type' <*> optional memoryLocation  <*> identifier

-- statements

statement :: Parser Statement
statement = backtrack
  [ varDeclStmt
  , returnStmt
  , assignmentStmt
  , blockStmt
  , ifStmt
  , expressionStmt
  , whileStmt
  , forStmt
  , forEachStmt
  , breakStmt
  , continueStmt
  ]

expressionStmt :: Parser Statement
expressionStmt = endsIn ";" stmt
  where stmt = ExpressionStmt <$> expression

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
assignmentSymbol = reserved' "<-" Storage <|> reserved' ":=" Memory

whileStmt :: Parser Statement
whileStmt = WhileStmt <$> cond <*> body
  where cond = reserved "while" *> reserved "(" *> expression <* reserved ")"
        body = statement

forStmt :: Parser Statement
forStmt = ForStmt <$> decl <*> cond <*> step <*> body
  where decl = reserved "for" *> reserved "(" *> optional stmt <* reserved ";"
        stmt = VarDeclStmt <$> arg <*> optional ((,) <$> assignmentSymbol <*> expression)
        cond = optional expression <* reserved ";"
        step = optional expression <* reserved ")"
        body = statement

forEachStmt :: Parser Statement
forEachStmt = ForEachStmt <$> id <*> expression <*> body
  where id        = reserved "for" *> identifier <* reserved "in"
        body      = statement

breakStmt :: Parser Statement
breakStmt = endsIn ";" stmt 
  where stmt = reserved' "break" BreakStmt

continueStmt :: Parser Statement
continueStmt = endsIn ";" stmt 
  where stmt = reserved' "continue" ContinueStmt

blockStmt :: Parser Statement
blockStmt = BlockStmt <$> block (many statement)

ifStmt :: Parser Statement
ifStmt = IfStmt <$> cond <*> statement <*> elseBranch
  where cond       = reserved "if" *> parens expression
        elseBranch = optional $ reserved "else" *> statement

-- expressions

expression :: Parser Expression
expression =
  baseExpr
  `chainl1` multiplicativeOps
  `chainl1` additiveOps
  `chainl1` shiftOps
  `chainl1` comparisonOps
  `chainl1` equalityOps
  `chainl1` logicalOps
  `chainl1` bitwiseOps

baseExpr :: Parser Expression
baseExpr = backtrack
  [ memberExpr
  , factor
  , unaryExpr <*> expression
  ]

memberExpr :: Parser Expression
memberExpr = do
  struct <- factor
  members <- many1 $ Left <$> member <|> Right <$> subscript
  return $ Prelude.foldl (\exp -> either (MemberAccessE exp) (SubscriptE exp)) struct members
  where member    = reserved "." *> identifier
        subscript = brackets expression

unaryExpr :: Parser UnaryExpression
unaryExpr = choice $ uncurry mkUnaryExpr <$>
  [ ("++", IncrementOp)
  , ("--", DecrementOp)
  , ("-", ArithmeticNegationOp)
  , ("!", LogicalNegationOp)
  , ("~", BitwiseNegationOp)
  ]

factor :: Parser Expression
factor =
  parens expression
  <|> try functionCallExpr
  <|> literalExpr
  <|> identifierExpr

literalExpr :: Parser Expression
literalExpr = LiteralE <$> literal

identifierExpr :: Parser Expression
identifierExpr = IdentifierE <$> identifier

functionCallExpr :: Parser Expression
functionCallExpr = FunctionCallE <$> identifier <*> argList
  where argList = parens (sepBy expression comma)

mkBinaryExpr :: Text -> BinaryOp -> Parser BinaryExpression
mkBinaryExpr sym op = reserved' sym (BinaryE op)

mkUnaryExpr :: Text -> UnaryOp -> Parser UnaryExpression
mkUnaryExpr sym op = reserved' sym (UnaryE op)

-- operators

multiplicativeOps :: Parser BinaryExpression
multiplicativeOps = choice $ uncurry mkBinaryExpr <$>
  [ ("*", MultiplicationOp)
  , ("/", DivisionOp)
  , ("%", ModuloOp)
  ]

additiveOps :: Parser BinaryExpression
additiveOps = choice $ uncurry mkBinaryExpr <$> [ ("+", AdditionOp), ("-", SubtractionOp) ]

shiftOps :: Parser BinaryExpression
shiftOps = choice $ uncurry mkBinaryExpr <$> [ (">>", RightShiftOp), ("<<", LeftShiftOp) ]

comparisonOps :: Parser BinaryExpression
comparisonOps = choice $ try . uncurry mkBinaryExpr <$>
    [ ("<=", LessThanEqualOp)
    , (">=", GreaterThanEqualOp)
    , ("<", LessThanOp)
    , (">", GreaterThanOp)
    ]

equalityOps :: Parser BinaryExpression
equalityOps = choice $ uncurry mkBinaryExpr <$> [ ("!=", InequalityOp), ("==", EqualityOp) ]

logicalOps :: Parser BinaryExpression
logicalOps = choice $ uncurry mkBinaryExpr <$> [ ("&&", ConjunctionOp), ("||", DisjunctionOp) ]

bitwiseOps :: Parser BinaryExpression
bitwiseOps = choice $ uncurry mkBinaryExpr <$>
  [ ("&", BitwiseConjunctionOp)
  , ("|", BitwiseDisjunctionOp)
  , ("^", BitwiseExclDisjunctionOp)
  ]

-- types

type' :: Parser Type
type' = backtrack [mappingType, arrayType, primitiveType]

primitiveType :: Parser Type
primitiveType =  backtrack
  [ reserved' "address" AddressT
  , reserved' "bool" BoolT
  , reserved' "string" StringT
  , chunk "uint" *> sizedType UIntT uintPredicates
  , chunk "bytes" *> sizedType BytesT bytesPredicates
  , reserved' "bytes" BytesDynamicT
  , UserDefinedT <$> identifier
  ]

arrayType :: Parser Type
arrayType = ArrayT <$> primitiveType <*> dimensions
  where dimensions = many $ reserved "[" *> optional uintRaw <* reserved "]"

mappingType :: Parser Type
mappingType = MappingT <$> key <*> value
  where key   = reserved "mapping" *> reserved "(" *> type' <* reserved "=>"
        value = type' <* reserved ")"

sizedType :: (Int -> Type) -> [Int -> Bool] -> Parser Type
sizedType c ps = c <$> typeSize ps

typeSize :: [Int -> Bool] -> Parser Int
typeSize ps = do
  size <- uintRaw
  guard $ testPredicates size ps
  return size

-- identifiers

identifier :: Parser Text
identifier = lexeme' $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum

-- literals

literal :: Parser Value
literal = backtrack
  [ addressLit
  , boolLit
  , bytesLit
  , stringLit
  , uintLit
  , arrayLit
  , structLit
  ]

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

arrayLit :: Parser Value
arrayLit = ArrayV <$> brackets values
  where values = sepBy1 expression comma

structLit :: Parser Value
structLit = StructV <$> block fields
  where fields = sepBy1 field comma
        field  = (,) <$> identifier <*> (reserved ":" *> expression)

-- raw values

stringRaw :: Parser Text
stringRaw = pack <$> (char '\"' *> manyTill charLiteral (char '\"'))

uintRaw :: Parser Int
uintRaw = lexeme' decimal

boolRaw :: Parser Bool
boolRaw = lexeme' $ True <$ chunk "true" <|> False <$ chunk "false"

hexRaw :: Parser Text
hexRaw = chunk "0x" *> takeWhile1P Nothing isHexDigit

bytesRaw :: Parser Text
bytesRaw = lexeme' hexRaw
