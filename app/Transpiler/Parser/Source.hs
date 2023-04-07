{-# LANGUAGE OverloadedStrings #-}

module Transpiler.Parser.Source where

import Control.Monad (guard, when)
import Data.Char (isHexDigit, isAlpha, isAlphaNum)
import Data.Text hiding (elem)
import Transpiler.Parser.Base
import Transpiler.Parser.Types
import Transpiler.Parser.Utils
import Transpiler.Iaspis.Grammar hiding (moduleDecl, fieldProxyKind, functionHeader, overrideSpecifier)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, charLiteral)
import Data.Functor (($>))
import Data.Either
import Prelude hiding (Enum)


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
decl = backtrack
  [ ContractDecl <$> immutableContract
  , ProxyDecl <$> proxyContract
  , FacetDecl <$> facetContract
  , StructDecl <$> struct
  , EnumDecl <$> enum
  ]

immutableContract :: Parser ImmutableContract
immutableContract = do
  name <- reserved "contract" *> identifier
  memberList <- block $ many (backtrack [Left <$> endsIn ";" contractFieldDecl, Right <$> function])
  return $ ImmutableContract name (lefts memberList) (rights memberList)

proxyContract :: Parser ProxyContract
proxyContract = ProxyContract <$> proxyKind' <*> name  <*> facetList <*> memberList
  where proxyKind' = proxyKind <* reserved "proxy"
        name       = identifier <* reserved "for"
        facetList  = sepBy identifier comma
        memberList = block $ many (endsIn ";" contractFieldDecl)

facetContract :: Parser FacetContract
facetContract = FacetContract <$> name <*> proxy <*> memberList
  where name       = reserved "facet" *> identifier
        proxy  = reserved "to" *> identifier
        memberList = block $ many function

struct :: Parser Struct
struct = Struct <$> name <*> fields
  where name      = reserved "struct" *> identifier
        fields    = block . many $ endsIn ";" structFieldDecl

enum :: Parser Enum
enum = Enum <$> name <*> fields
  where name   = reserved "enum" *> identifier
        fields = block $ sepBy1 identifier comma

-- field declarations

fnArgDecl :: Parser FunctionArg
fnArgDecl =
  FunctionArg
  <$> typeWithLoc
  <*> identifier

declArg :: Parser DeclArg
declArg =
  DeclArg
  <$> option View mutability
  <*> typeWithLoc
  <*> identifier

contractFieldDecl :: Parser Field
contractFieldDecl =
  Field
  <$> optional fieldProxyKind
  <*> optional visibility
  <*> mutability
  <*> storageType
  <*> pure Storage
  <*> identifier
  <*> optional (reserved' "<-" Storage *> expression)

structFieldDecl :: Parser StructField
structFieldDecl = StructField <$> type' <*> identifier

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
  = FunctionHeader <$> visibility <*> payability <*> mutability <*> name <*> argList <*> returnArg <*> overrideSpecifier
  where name       = reserved "fn" *> identifier
        argList    = parens (sepBy fnArgDecl comma)
        returnArg  = FunctionArg <$> option UnitT (reserved "->" *> typeWithLoc) <*> pure ""
        -- returnArg = option UnitT $ reserved "->" *> type'

constructorHeader :: Parser FunctionHeader
constructorHeader
  = FunctionHeader Public <$> payability <*> pure Mutable <*> name <*> argList <*> returnArg <*> pure False
  where name    = lexeme' "constructor"
        argList = parens (sepBy fnArgDecl comma)
        returnArg  = pure (FunctionArg UnitT "")

receiveHeader :: Parser FunctionHeader
receiveHeader
  = FunctionHeader External Payable <$> mutability <*> name <*> argList <*> returnArg <*> pure False
  where name    = lexeme' "receive"
        argList = parens spaceOrComment $> []
        returnArg  = pure (FunctionArg UnitT "")

fallbackHeader :: Parser FunctionHeader
fallbackHeader
  = FunctionHeader External NonPayable <$> mutability <*> name <*> argList <*> returnArg <*> pure False
  where name    = lexeme' "fallback"
        argList = parens spaceOrComment $> []
        returnArg  = pure (FunctionArg UnitT "")

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
  , whileStmt
  , breakStmt
  , continueStmt
  ]

expressionStmt :: Parser Statement
expressionStmt = endsIn ";" stmt
  where stmt = ExpressionStmt <$> expression

varDeclStmt :: Parser Statement
varDeclStmt = endsIn ";" stmt
  where stmt = VarDeclStmt <$> declArg <*> assignmentSymbol <*> expression

returnStmt :: Parser Statement
returnStmt = endsIn ";" stmt
  where stmt = ReturnStmt <$> (reserved "return" *> optional expression)

assignmentStmt :: Parser Statement
assignmentStmt = endsIn ";" stmt
  where stmt = AssignmentStmt <$> lvalueExpr <*> assignmentSymbol <*> expression

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

whileStmt :: Parser Statement
whileStmt = WhileStmt <$> cond <*> statement
  where cond = reserved "while" *> parens expression

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

lvalueExpr :: Parser Expression
lvalueExpr = backtrack
  [ memberExpr
  , IdentifierE <$> identifier
  ]

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
  where member = reserved "." *> identifier
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
  <|> try instantiationExpr
  <|> try functionCallExpr
  <|> try literalExpr
  <|> try identifierExpr

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


-- type with no data location, used for structs
type' :: Parser Type
type' = backtrack
  [ ArrayT <$> nonArrayType <*> arrayDims <*> pure Nothing
  , reserved' "address" AddressT
  , reserved' "bool" BoolT
  , reserved' "string" (StringT Nothing)
  , reserved' "uint" UIntT
  , chunk "bytes" *> sizedType BytesT bytesPredicates
  , reserved' "bytes" (BytesDynT Nothing)
  , mappingType
  , UserDefinedT <$> identifier <*> pure Nothing
  ]

nonArrayType :: Parser Type 
nonArrayType = backtrack
  [ reserved' "address" AddressT
  , reserved' "bool" BoolT
  , reserved' "string" (StringT Nothing)
  , reserved' "uint" UIntT
  , chunk "bytes" *> sizedType BytesT bytesPredicates
  , reserved' "bytes" (BytesDynT Nothing)
  , UserDefinedT <$> identifier <*> pure Nothing
  ]

mappingKeyType :: Parser Type
mappingKeyType = backtrack
  [ reserved' "address" AddressT
  , reserved' "bool" BoolT
  , reserved' "string" (StringT Nothing)
  , reserved' "uint" UIntT
  , chunk "bytes" *> sizedType BytesT bytesPredicates
  , reserved' "bytes" (BytesDynT Nothing)
  ]

-- used for contract & proxy field declarations
storageType :: Parser Type
storageType = backtrack
  [ ArrayT <$> nonArrayType <*> arrayDims <*> pure (Just Storage)
  , reserved' "address" AddressT
  , reserved' "bool" BoolT
  , reserved' "string" (StringT (Just Storage))
  , reserved' "uint" UIntT
  , chunk "bytes" *> sizedType BytesT bytesPredicates
  , reserved' "bytes" (BytesDynT (Just Storage))
  , mappingType
  , UserDefinedT <$> identifier <*> pure (Just Storage)
  ]

typeWithLoc :: Parser Type
typeWithLoc = backtrack
  [ ArrayT <$> nonArrayType <*> arrayDims <*> (Just <$> memoryLocation)
  , reserved' "address" AddressT
  , reserved' "bool" BoolT
  , StringT <$> (reserved "string" *> (Just <$> memoryLocation))
  , reserved' "uint" UIntT
  , chunk "bytes" *> sizedType BytesT bytesPredicates
  , BytesDynT <$> (reserved "bytes" *> (Just <$> memoryLocation))
  , mappingType
  , UserDefinedT <$> userDefinedId <*> option Nothing (Just <$> memoryLocation)
  ]

mappingType :: Parser Type
mappingType = 
  MappingT 
  <$> (reserved "mapping" *> reserved "(" *> mappingKeyType) 
  <*> (reserved "=>" *> type' <* reserved ")")

arrayDims :: Parser [Maybe Int]
arrayDims = many1 $ reserved "[" *> optional uintRaw <* reserved "]"

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

userDefinedId :: Parser Text
userDefinedId = do
  id <- lexeme' $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum
  when (id `elem` reservedTypeIds) (fail . show $ id <> "is a reserved type")
  return id
-- literals

literal :: Parser Value
literal = backtrack
  [ structLit
  , boolLit
  , bytesLit
  , stringLit
  , uintLit
  , enumLit
  , arrayLit
  ]

boolLit :: Parser Value
boolLit = BoolV <$> boolRaw

bytesLit :: Parser Value
bytesLit = BytesV <$> bytesRaw

stringLit :: Parser Value
stringLit = StringV <$> stringRaw

uintLit :: Parser Value
uintLit = UIntV <$> uintRaw

structLit :: Parser Value
structLit = StructV <$> structValue
  where structValue = StructValue <$> identifier <*> braces structMembers
        structMembers = sepBy structValMember comma
        structValMember = StructValueMember <$> identifier <*> (reserved "=" *> expression)

enumLit :: Parser Value
enumLit = EnumV <$> identifier <*> (reserved "::" *> identifier)

arrayLit :: Parser Value
arrayLit = ArrayV <$> brackets (sepBy expression comma)

-- raw values

stringRaw :: Parser Text
stringRaw = lexeme' $ pack <$> (char '\"' *> manyTill charLiteral (char '\"'))

uintRaw :: Parser Int
uintRaw = lexeme' decimal

boolRaw :: Parser Bool
boolRaw = lexeme' $ True <$ chunk "true" <|> False <$ chunk "false"

hexRaw :: Parser Text
hexRaw = chunk "0x" *> takeWhile1P Nothing isHexDigit

bytesRaw :: Parser Text
bytesRaw = lexeme' hexRaw

-- 

reservedTypeIds :: [Text]
reservedTypeIds =
  [ "uint"
  , "string"
  , "bool"
  , "address"
  , "bytes"
  ]