-- {-# LANGUAGE OverloadedStrings #-}
-- module Parser where

-- import Data.Functor (($>))
-- import Data.Text
-- import Data.Void
-- import Data.Word
-- import Text.Megaparsec
-- import Data.Char (isDigit, isHexDigit, isAlphaNum, isAlpha)
-- import Text.Megaparsec.Char.Lexer (decimal, hexadecimal, skipLineComment, skipBlockComment, space, lexeme, charLiteral)
-- import Numeric (showHex)
-- import Prelude hiding (length, lex, Enum)
-- import Control.Monad (guard, void, when)
-- import Text.Megaparsec.Char (char, space1)
-- import Source
-- import Data.Functor.Contravariant (Predicate (getPredicate, Predicate))
-- import Utils (uintPredicates, testPredicates, bytesPredicates)

-- type Parser = Parsec Void Text
-- type BinaryExpression = Expression -> Expression -> Expression
-- type UnaryExpression = Expression -> Expression

-- lineComment :: Parser ()
-- lineComment = skipLineComment "//"

-- blockComment :: Parser ()
-- blockComment = skipBlockComment "/*" "*/"

-- whitespace :: Parser ()
-- whitespace = space1

-- spaceOrComment :: Parser ()
-- spaceOrComment = space whitespace lineComment blockComment

-- reserved :: Text -> Parser ()
-- reserved = void . lex . chunk

-- block :: Parser a -> Parser a
-- block = between (reserved "{") (reserved "}")

-- backtrack :: [Parser a] -> Parser a
-- backtrack = choice . (<$>) try

-- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- chainl1 p op = do
--     x <- p
--     rest x
--     where
--         rest x = do
--             f <- op
--             y <- p
--             rest (f x y)
--             <|> return x


-- many1 :: Parser a -> Parser [a]
-- many1 p             = do{ x <- p; xs <- many p; return (x:xs) }


-- source' :: Parser Source
-- source' = Source <$> module' <*> many (try importStmt) <*> many decl

-- source :: Parser Source
-- source = source' <* eof

-- module' :: Parser Module
-- module' = Module <$> (endsIn ";" $ (reserved "module" *> identifier))

-- semicolon :: Parser ()
-- semicolon = reserved ";"

-- comma :: Parser ()
-- comma = reserved ","

-- endsIn :: Text -> Parser a -> Parser a
-- endsIn t = flip (<*) (reserved t)

-- keyword :: Text -> a -> Parser a
-- keyword k = (<$ reserved k)

-- importStmt :: Parser Import
-- importStmt = Import <$> endsIn ";" import'
--     where import' = reserved "import" *> identifier

-- decl :: Parser Declaration
-- decl = backtrack [struct, enum, contract]

-- struct :: Parser Declaration
-- struct = StructDecl <$> (Struct <$> name <*> fields)
--     where name      = reserved "struct" *> identifier
--           fields    = block $ many fieldDecl
--           fieldDecl = endsIn ";" arg

-- enum :: Parser Declaration
-- enum = EnumDecl <$> (Enum <$> name <*> fields)
--     where name   = reserved "enum" *> identifier
--           fields = block $ sepBy1 identifier comma

-- contract :: Parser Declaration
-- contract = ContractDecl <$> backtrack [immutableContract, proxyContract, facetContract]

-- immutableContract :: Parser Contract
-- immutableContract = ImmutableContract <$> name <*> inheritanceList <*> memberList
--     where name            = reserved "contract" *> identifier
--           inheritanceList = sepBy identifier comma

-- proxyContract :: Parser Contract
-- proxyContract = ProxyContract <$> kind' <*> name  <*> facetList <*> memberList
--     where kind'      = kind <* reserved "proxy"
--           name       = identifier <* reserved "for"
--           facetList  = sepBy identifier comma

-- kind :: Parser ProxyKind
-- kind = ProxyOpen <$ reserved "open" <|> ProxyClosed <$ reserved "closed"

-- facetContract :: Parser Contract
-- facetContract = FacetContract <$> name <*> proxyList <*> memberList
--     where name       = reserved "facet" *> identifier
--           proxyList  = reserved "to" *> sepBy identifier comma

-- memberList :: Parser [MemberDecl]
-- memberList = block $ many memberDecl

-- memberDecl :: Parser MemberDecl
-- memberDecl = backtrack [fieldDecl, functionDecl]

-- fieldDecl :: Parser MemberDecl
-- fieldDecl = endsIn ";" field
--     where field      = FieldDecl <$> optional proxyKind <*> optional visibility <*> modifiers <*> type' <*> identifier
--           modifiers  = many modifier

-- forFieldDecl :: Parser MemberDecl
-- forFieldDecl = FieldDecl Nothing Nothing [] <$> type' <*> identifier

-- proxyKind :: Parser ProxyMemberKind
-- proxyKind = reserved "@" *> backtrack
--     [ keyword "*" SharedProxyMember
--     , UniqueProxyMember <$> identifier
--     ]

-- visibility :: Parser MemberVisibility
-- visibility = backtrack
--     [ keyword "pub" Public
--     , keyword "pvt" Private
--     , keyword "int" Internal
--     , keyword "ext" External
--     ]

-- modifier :: Parser FieldModifier
-- modifier = backtrack [keyword "const" ConstMod]

-- functionDecl :: Parser MemberDecl
-- functionDecl = FunctionDecl <$> visibility <*> payability <*> functionKind' <*> identifier <*> functionSig' <*> body
--     where body = many statement

-- payability :: Parser PayabilityKind
-- payability = backtrack
--     [ keyword "$" Payable
--     , keyword "!" NonPayable
--     ]

-- functionKind' :: Parser FunctionKind
-- functionKind' = backtrack
--     [ keyword "fn" Function
--     , keyword "proc" Procedure
--     ]

-- functionSig' :: Parser FunctionSignature
-- functionSig' = FunctionSignature <$> argList <*> returnType
--     where argList = between (reserved "(") (reserved ")") (sepBy arg comma)
--           returnType = reserved "=>" *> type'

-- arg :: Parser Arg
-- arg = Arg <$> type' <*> optional memoryLocation  <*> identifier

-- memoryLocation :: Parser MemoryLocation
-- memoryLocation = backtrack
--     [ keyword "storage" Storage
--     , keyword "memory" Memory
--     ]

-- identifier :: Parser Text
-- identifier = lex $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum

-- sizedType :: (Int -> Type) -> [Int -> Bool] -> Parser Type
-- sizedType c ps = c <$> typeSize ps

-- type' :: Parser Type
-- type' = backtrack [mappingType, arrayType, primitiveType]

-- primitiveType :: Parser Type
-- primitiveType =  backtrack
--     [ keyword "address" AddressT
--     , keyword "bool" BoolT
--     , keyword "string" StringT
--     , chunk "uint" *> sizedType UIntT uintPredicates
--     , chunk "bytes" *> sizedType BytesT bytesPredicates
--     , keyword "bytes" BytesDynamicT
--     , UserDefinedT <$> identifier
--     ]

-- arrayType :: Parser Type
-- arrayType = ArrayT <$> primitiveType <*> dimensions
--     where dimensions = many $ reserved "[" *> optional uintRaw <* reserved "]"

-- mappingType :: Parser Type
-- mappingType = MappingT <$> key <*> value
--     where key   = reserved "mapping" *> reserved "(" *> type' <* reserved "=>"
--           value = type' <* reserved ")"

-- typeSize :: [Int -> Bool] -> Parser Int
-- typeSize ps = do
--     size <- uintRaw
--     guard $ testPredicates size ps
--     return size

-- statement :: Parser Statement
-- statement = backtrack
--     [ varDeclStmt
--     , returnStmt
--     , assignmentStmt
--     , blockStmt
--     , ifStmt
--     , expressionStmt
--     , whileStmt
--     , forStmt
--     , forEachStmt
--     , breakStmt
--     , continueStmt
--     ]

-- expressionStmt :: Parser Statement
-- expressionStmt = endsIn ";" stmt
--     where stmt = ExpressionStmt <$> expression

-- varDeclStmt :: Parser Statement
-- varDeclStmt = endsIn ";" stmt
--     where stmt = VarDeclStmt <$> arg <*> optional ((,) <$> assignmentSymbol <*> expression)

-- returnStmt :: Parser Statement
-- returnStmt = endsIn ";" stmt
--     where stmt = ReturnStmt <$> (reserved "return" *> expression)

-- assignmentStmt :: Parser Statement
-- assignmentStmt = endsIn ";" stmt
--     where stmt = AssignmentStmt <$> identifier <*> assignmentSymbol <*> expression

-- assignmentSymbol :: Parser MemoryLocation
-- assignmentSymbol = reserved "<-" $> Storage <|> reserved ":=" $> Memory

-- whileStmt :: Parser Statement
-- whileStmt = WhileStmt <$> cond <*> body
--     where cond = reserved "while" *> reserved "(" *> expression <* reserved ")"
--           body = statement

-- forStmt :: Parser Statement
-- forStmt = ForStmt <$> decl <*> cond <*> step <*> body
--     where decl = reserved "for" *> reserved "(" *> optional stmt <* reserved ";"
--           stmt = VarDeclStmt <$> arg <*> optional ((,) <$> assignmentSymbol <*> expression)
--           cond = optional expression <* reserved ";"
--           step = optional expression <* reserved ")"
--           body = statement

-- forEachStmt :: Parser Statement
-- forEachStmt = ForEachStmt <$> id <*> container <*> body
--     where id = reserved "for" *> identifier <* reserved "in"
--           container = expression
--           body = statement

-- breakStmt :: Parser Statement
-- breakStmt = endsIn ";" stmt 
--     where stmt = reserved "break" $> BreakStmt

-- continueStmt :: Parser Statement
-- continueStmt = endsIn ";" stmt 
--     where stmt = reserved "continue" $> ContinueStmt

-- blockStmt :: Parser Statement
-- blockStmt = BlockStmt <$> block (many statement)

-- ifStmt :: Parser Statement
-- ifStmt = IfStmt <$> cond <*> ifBranch <*> elseBranch
--     where cond = reserved "if" *> between (reserved "(") (reserved ")") expression
--           ifBranch = statement
--           elseBranch = optional $ reserved "else" *> statement

-- mkBinaryExpr :: Text -> BinaryOp -> Parser BinaryExpression
-- mkBinaryExpr sym op = reserved sym $> BinaryE op

-- mkUnaryExpr :: Text -> UnaryOp -> Parser UnaryExpression
-- mkUnaryExpr sym op = reserved sym $> UnaryE op

-- expression :: Parser Expression
-- expression =
--     baseExpr
--     `chainl1` multiplicativeOps
--     `chainl1` additiveOps
--     `chainl1` shiftOps
--     `chainl1` comparisonOps
--     `chainl1` equalityOps
--     `chainl1` logicalOps
--     `chainl1` bitwiseOps

-- baseExpr :: Parser Expression
-- baseExpr = backtrack
--     [ memberExpr
--     , factor
--     , unaryExpr <*> expression
--     ]

-- memberExpr :: Parser Expression
-- memberExpr = do
--     struct <- factor
--     members <- many1 $ Left <$> member <|> Right <$> subscript
--     return $ Prelude.foldl (\exp -> either (MemberAccessE exp) (SubscriptE exp)) struct members
--     where member = reserved "." *> identifier
--           subscript = between (reserved "[") (reserved "]") expression

-- multiplicativeOps :: Parser BinaryExpression
-- multiplicativeOps = choice $ uncurry mkBinaryExpr <$>
--     [ ("*", MultiplicationOp)
--     , ("/", DivisionOp)
--     , ("%", ModuloOp)
--     ]

-- additiveOps :: Parser BinaryExpression
-- additiveOps = choice $ uncurry mkBinaryExpr <$> [ ("+", AdditionOp), ("-", SubtractionOp) ]

-- shiftOps :: Parser BinaryExpression
-- shiftOps = choice $ uncurry mkBinaryExpr <$> [ (">>", RightShiftOp), ("<<", LeftShiftOp) ]

-- comparisonOps :: Parser BinaryExpression
-- comparisonOps = choice $ try . uncurry mkBinaryExpr <$>
--     [ ("<=", LessThanEqualOp)
--     , (">=", GreaterThanEqualOp)
--     , ("<", LessThanOp)
--     , (">", GreaterThanOp)
--     ]

-- equalityOps :: Parser BinaryExpression
-- equalityOps = choice $ uncurry mkBinaryExpr <$> [ ("!=", InequalityOp), ("==", EqualityOp) ]

-- logicalOps :: Parser BinaryExpression
-- logicalOps = choice $ uncurry mkBinaryExpr <$> [ ("&&", ConjunctionOp), ("||", DisjunctionOp) ]

-- bitwiseOps :: Parser BinaryExpression
-- bitwiseOps = choice $ uncurry mkBinaryExpr <$>
--     [ ("&", BitwiseConjunctionOp)
--     , ("|", BitwiseDisjunctionOp)
--     , ("^", BitwiseExclDisjunctionOp)
--     ]

-- factor :: Parser Expression
-- factor =
--     between (reserved "(") (reserved ")") expression
--     <|> try functionCallExpr
--     <|> literalExpr
--     <|> identifierExpr

-- literalExpr :: Parser Expression
-- literalExpr = LiteralE <$> literal

-- identifierExpr :: Parser Expression
-- identifierExpr = IdentifierE <$> identifier

-- functionCallExpr :: Parser Expression
-- functionCallExpr = FunctionCallE <$> identifier <*> argList
--     where argList = between (reserved "(") (reserved ")") args
--           args = sepBy expression (reserved ",")

-- ternaryExpr :: Parser Expression
-- ternaryExpr = TernaryE <$> cond <*> exp1 <*> exp2
--     where cond = factor <* reserved "?"
--           exp1 = expression <* reserved ":"
--           exp2 = expression

-- unaryExpr :: Parser UnaryExpression
-- unaryExpr = choice $ uncurry mkUnaryExpr <$>
--     [ ("++", IncrementOp)
--     , ("--", DecrementOp)
--     , ("-", ArithmeticNegationOp)
--     , ("!", LogicalNegationOp)
--     , ("~", BitwiseNegationOp)
--     ]

-- lex :: Parser a -> Parser a
-- lex = lexeme spaceOrComment

-- stringRaw :: Parser Text
-- stringRaw = pack <$> (char '\"' *> manyTill charLiteral (char '\"'))

-- uintRaw :: Parser Int
-- uintRaw = lex decimal

-- boolRaw :: Parser Bool
-- boolRaw = lex $ True <$ chunk "true" <|> False <$ chunk "false"

-- hexRaw :: Parser Text
-- hexRaw = chunk "0x" *> takeWhile1P Nothing isHexDigit

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

-- arrayLit :: Parser Value
-- arrayLit = ArrayV <$> between (reserved "[") (reserved "]") values
--     where values = sepBy1 expression (reserved ",")

-- structLit :: Parser Value
-- structLit = StructV <$> between (reserved "{") (reserved "}") fields
--     where fields = sepBy1 field (reserved ",")
--           field = (,) <$> identifier <*> (reserved ":" *> expression)

-- literal :: Parser Value
-- literal = backtrack
--     [ addressLit
--     , boolLit
--     , bytesLit
--     , stringLit
--     , uintLit
--     , arrayLit
--     , structLit
--     ]