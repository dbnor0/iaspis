{-# LANGUAGE OverloadedStrings #-}

module Parser.Source where

import Control.Monad (guard)
import Data.Char (isHexDigit, isAlpha, isAlphaNum)
import Data.Text
import Parser.Base
import Parser.Types
import Parser.Utils
import Iaspis.Grammar hiding (moduleDecl, fieldProxyKind, functionHeader, overrideSpecifier)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, charLiteral)
import Data.Functor (($>))
import Data.Either


module' :: Parser Module
module' = module'' <* eof
  where module'' = Module <$> moduleDecl <*> many importStmt <*> many decl

-- top level elements

moduleDecl :: Parser Identifier
moduleDecl = endsIn ";" (reserved "module" *> identifier)

importStmt :: Parser Import
importStmt = endsIn ";" stmt
  where stmt = Import <$> (reserved "import" *> sepBy1 identifier comma) <*> (reserved "from" *> identifier)

decl :: Parser Declaration
decl = contract

contract :: Parser Declaration
contract = ContractDecl <$> backtrack [immutableContract, proxyContract, facetContract]

immutableContract :: Parser Contract
immutableContract = do
  name <- reserved "contract" *> identifier
  memberList <- block $ many (backtrack [Left <$> endsIn ";" contractFieldDecl, Right <$> function])
  return $ ImmutableContract name (lefts memberList) (rights memberList)

proxyContract :: Parser Contract
proxyContract = ProxyContract <$> proxyKind' <*> name  <*> facetList <*> memberList
  where proxyKind' = proxyKind <* reserved "proxy"
        name       = identifier <* reserved "for"
        facetList  = sepBy identifier comma
        memberList = block $ many (endsIn ";" contractFieldDecl)

facetContract :: Parser Contract
facetContract = FacetContract <$> name <*> proxy <*> memberList
  where name       = reserved "facet" *> identifier
        proxy  = reserved "to" *> identifier
        memberList = block $ many function

-- field declarations

fnFieldDecl :: Parser Field
fnFieldDecl =
  Field
  <$> optional fieldProxyKind
  <*> optional visibility
  <*> mutability
  <*> type'
  <*> option Memory memoryLocation
  <*> identifier

contractFieldDecl :: Parser Field
contractFieldDecl =
  Field
  <$> optional fieldProxyKind
  <*> optional visibility
  <*> mutability
  <*> type'
  <*> pure Storage
  <*> identifier

function :: Parser Function
function = Function <$> functionHeader <*> body
  where body = block $ many1 statement

functionHeader :: Parser FunctionHeader
functionHeader = backtrack
  [ constructorHeader
  , receiveHeader
  , fallbackHeader
  , userDefinedHeader
  ]

userDefinedHeader :: Parser FunctionHeader
userDefinedHeader
  = FunctionHeader <$> visibility <*> payability <*> mutability <*> name <*> argList <*> returnType <*> overrideSpecifier
  where name       = reserved "fn" *> identifier
        argList    = parens (sepBy fnFieldDecl comma)
        returnType = option UnitT $ reserved "->" *> type'

constructorHeader :: Parser FunctionHeader
constructorHeader
  = FunctionHeader Public <$> payability <*> pure Mutable <*> name <*> argList <*> pure UnitT <*> pure False
  where name    = lexeme' "constructor"
        argList = parens (sepBy fnFieldDecl comma)

receiveHeader :: Parser FunctionHeader
receiveHeader
  = FunctionHeader External Payable <$> mutability <*> name <*> argList <*> pure UnitT <*> pure False
  where name    = lexeme' "receive"
        argList = parens spaceOrComment $> []

fallbackHeader :: Parser FunctionHeader
fallbackHeader
  = FunctionHeader External NonPayable <$> mutability <*> name <*> argList <*> pure UnitT <*> pure False
  where name    = lexeme' "fallback"
        argList = parens spaceOrComment $> []

-- syntax elements

proxyKind :: Parser ProxyKind
proxyKind = reserved' "open" ProxyOpen <|> reserved' "closed" ProxyClosed

fieldProxyKind :: Parser ProxyMemberKind
fieldProxyKind = reserved "@" *> backtrack
  [ reserved' "*" SharedProxyMember
  , UniqueProxyMember <$> identifier
  ]

visibility :: Parser MemberVisibility
visibility = backtrack
  [ reserved' "public" Public
  , reserved' "private" Private
  , reserved' "internal" Internal
  , reserved' "external" External
  ]

payability :: Parser PayabilityKind
payability = option NonPayable (reserved' "pay" Payable)

mutability :: Parser Mutability
mutability = option View (reserved' "mut" Mutable)

memoryLocation :: Parser MemoryLocation
memoryLocation = backtrack
  [ reserved' "storage" Storage
  , reserved' "memory" Memory
  ]

overrideSpecifier :: Parser Bool
overrideSpecifier = option False (reserved' "override" True)

-- statements

statement :: Parser Statement
statement = backtrack
  [ varDeclStmt
  , returnStmt
  , assignmentStmt
  , expressionStmt
  , blockStmt
  , ifStmt
  , breakStmt
  , continueStmt
  ]

expressionStmt :: Parser Statement
expressionStmt = endsIn ";" stmt
  where stmt = ExpressionStmt <$> expression

varDeclStmt :: Parser Statement
varDeclStmt = endsIn ";" stmt
  where stmt = VarDeclStmt <$> fnFieldDecl <*> assignmentSymbol <*> expression

returnStmt :: Parser Statement
returnStmt = endsIn ";" stmt
  where stmt = ReturnStmt <$> (reserved "return" *> optional expression)

assignmentStmt :: Parser Statement
assignmentStmt = endsIn ";" stmt
  where stmt = AssignmentStmt <$> identifier <*> assignmentSymbol <*> expression

assignmentSymbol :: Parser MemoryLocation
assignmentSymbol = reserved' "<-" Storage <|> reserved' ":=" Memory

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
  [ factor
  , unaryExpr <*> expression
  ]


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
  <|> try instantiationExpr
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

instantiationExpr :: Parser Expression
instantiationExpr = InstantiationE <$> (reserved "new" *> identifier) <*> argList
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
type' 
  =   primitiveType
  <|> UserDefinedT <$> identifier

primitiveType :: Parser Type
primitiveType = backtrack
  [ reserved' "address" AddressT
  , reserved' "bool" BoolT
  , reserved' "string" StringT
  , chunk "uint" *> sizedType UIntT uintPredicates
  , chunk "bytes" *> sizedType BytesT bytesPredicates
  , reserved' "bytes" BytesDynamicT
  ]

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
