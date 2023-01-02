{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Codegen.Generate where

import Codegen.Types
import Data.Text as T
import Solidity.Grammar as S
import Data.Text.IO as T
import Control.Monad.State
import Data.List qualified as L
import Data.Function
import Lens.Micro.Platform
import Utils.Text
import Codegen.Utils ( genText )


type SolTextGen = State GenState SolText

genFile :: FilePath -> (FilePath, SolText) -> IO ()
genFile dir (name, code) = T.writeFile (dir <> "\\" <> name <> ".sol") code

genModule :: Module -> (FilePath, SolText)
genModule Module{ moduleId, imports, decls } = (T.unpack moduleId, code)
  where code = evalState genModule' (GenState 0)
        genModule' = do
          l <- genLicense
          is <- traverse genImport imports
          ds <- traverse genDecl decls
          return $ l <> T.concat is <> T.concat ds

genLicense :: SolTextGen
genLicense = return "// SPDX-License-Identifier: MIT\n"

genImport :: Identifier -> SolTextGen
genImport i = return $ "import \"./" <> i <> ".sol\";\n"

genDecl :: Declaration -> SolTextGen
genDecl = \case
  ContractDef cd -> genContract cd
  InterfaceDef id -> genInterface id
  LibraryDef ld -> genLibrary ld
  StructTypeDef sd -> genStruct sd
  EnumDef ed -> genEnum ed

genContract :: ContractDefinition -> SolTextGen
genContract (ContractDefinition as cId inhL b) = do
  let abstractSpec = if as then "abstract " else ""
      cs = T.concat $ L.intersperse ", " inhL
      inhCs = if not $ Prelude.null inhL then " is " <> cs else ""
      header = abstractSpec <> "contract " <> cId <> inhCs <> "{\n"
  es <- withIndent' genContractBodyElem b
  return $ header <> es <> "}\n"

genContractBodyElem :: ContractBodyElem -> SolTextGen
genContractBodyElem = \case
  StateVarDecl svd -> genStateVarDecl svd
  ConstructorDef f -> genFunction f
  FunctionDef f -> genFunction f
  FallbackDef f -> genFunction f
  ReceiveDef f -> genFunction f
  StructDef _ -> return ""

genStateVarDecl :: StateVarDeclaration -> SolTextGen
genStateVarDecl (StateVarDeclaration t v m id _) = genText decl
  where decl = genType t <> " " <> genVisibility v <> " " <> genModifier m <> " " <> id <> ";\n"

genFunction :: FunctionDefinition -> SolTextGen
genFunction f@FunctionDefinition{ functionBody } = do
  h <- genFunctionHeader f
  b <- withIndent' genStmt functionBody  
  cb <- genText "}\n\n"
  return $ h <> " {\n" <> b <> cb

genFunctionHeader :: FunctionDefinition -> SolTextGen
genFunctionHeader (FunctionDefinition id v m p vt o args rt _) = genText header
  where header = keyword id <> id <> argList <> vis <> " " <> mut <> " " <> pay <> " " <> virt <> " " <> ov <> rt'
        rt' = genReturnType rt
        argList = "(" <> T.concat (L.intersperse "," (genFunctionArg <$> args)) <> ") "
        vis = genVisibility v
        mut = genMutability m
        pay = genSpec "payable" p
        virt = genSpec "virtual" vt
        ov = genSpec "override" o
        keyword fn
          | fn `elem` ["constructor", "receive", "fallback"] = ""
          | otherwise = "function "

genFunctionArg :: FunctionArg -> SolText
genFunctionArg (FunctionArg t loc id) = genType t <> " " <> genLocation loc <> " " <> id

genReturnType :: [FunctionArg] -> SolText
genReturnType [FunctionArg (PrimitiveT UnitT) _ _] = ""
genReturnType [FunctionArg t _ _] = "returns (" <> genType t <> ")"
genReturnType args = "returns (" <> T.concat (L.intersperse "," (genType . functionArgType <$> args)) <> ")"

genStmt :: Statement -> SolTextGen
genStmt = \case
  BlockStmt stmts -> do
    ob <- genText "{\n"
    ss <- withIndent' genStmt stmts
    cb <- genText "}\n"
    return $ ob <> ss <> cb
  VarDeclStmt id ml e -> do
    let decl = id <> " " <> genLocation ml
        expr = maybe "" (\e -> " = " <> genExpr e) e
    genText $ decl <> expr <> ";\n"
  AssignmentStmt id e -> genText $ id <> " = " <> genExpr e <> ";\n"
  ExpressionStmt e -> genText $ genExpr e
  IfStmt cond b1 b2 -> do
    c <- genText $ "if(" <> genExpr cond <> ") "
    b1' <- withIndent $ genStmt b1
    b2' <- maybe (return "") genElse b2
    return $ c <> b1' <> b2'
    where genElse s = do
            e <- genText "else "
            b <- withIndent $ genStmt s
            genText $ e <> b
  ForStmt s c i b -> do
    s' <- genStmt s
    c' <- genStmt c
    let i' = genExpr i
    b' <- withIndent $ genStmt b
    genText $ "for(" <> s' <> "; " <> c' <> "; " <> i' <> ")" <> b'
  WhileStmt cond b -> do
    c <- genText $ "while(" <> genExpr cond <> ")"
    ss <- withIndent $ genStmt b
    cb <- genText "}"
    return $ c <> ss <> cb
  ContinueStmt -> genText "continue;\n"
  BreakStmt -> genText "break;\n"
  ReturnStmt e -> genText $ "return " <> maybe "" genExpr e <> ";\n"
  RevertStmt e -> genText $ "revert(" <> genExpr e <> ");\n"
  AssemblyStmt _ -> genText "asm"

genExpr :: Expression -> SolText
genExpr = \case
  LiteralE lit -> genLit lit
  IdentifierE id -> id
  InlineArrayE es -> "[" <> T.concat (L.intersperse "," (genExpr <$> es)) <> "]"
  MemberAccessE lv m -> genExpr lv <> "." <> genExpr m
  SubscriptE lv i -> genExpr lv <> "[" <> genExpr i <> "]"
  FunctionCallE id args -> genExpr id <> "(" <> T.concat (L.intersperse "," (genExpr <$> args)) <> ")"
  CastE t e -> genType t <> "(" <> genExpr e <> ")"
  BinaryE op e1 e2 -> genExpr e1 <> " " <> genBinaryOp op <> " " <> genExpr e2
  UnaryE op e -> genUnaryOp op <> " " <> genExpr e

genLit :: Literal -> SolText
genLit = \case
  StringLit v -> v
  NumberLit n -> showT n
  BooleanLit b -> T.toLower $ showT b
  HexLit v -> v

genBinaryOp :: BinaryOp -> SolText
genBinaryOp = \case
  AdditionOp -> "+"
  SubtractionOp -> "-"
  MultiplicationOp -> "*"
  DivisionOp -> "/"
  ModuloOp -> "%"
  ConjunctionOp -> "&&"
  DisjunctionOp -> "||"
  EqualityOp -> "=="
  InequalityOp -> "!="
  LessThanOp -> "<"
  GreaterThanOp -> ">"
  LessThanEqualOp -> "<="
  GreaterThanEqualOp -> ">="
  LeftShiftOp -> "<<"
  RightShiftOp -> ">>"
  BitwiseConjunctionOp -> "&"
  BitwiseDisjunctionOp -> "|"
  BitwiseExclDisjunctionOp -> "^"

genUnaryOp :: UnaryOp -> SolText
genUnaryOp = \case
  ArithmeticNegationOp -> "-"
  LogicalNegationOp -> "!"
  BitwiseNegationOp -> "~"
  IncrementOp -> "++"
  DecrementOp -> "--"

genInterface :: InterfaceDefinition -> SolTextGen
genInterface _ = return ""

genLibrary :: LibraryDefinition -> SolTextGen
genLibrary _ = return ""

genStruct :: StructDefinition -> SolTextGen
genStruct _ = return ""

genEnum :: EnumDefinition -> SolTextGen
genEnum _ = return ""

genType :: Type -> SolText
genType = \case
  PrimitiveT AddressT -> "address"
  PrimitiveT PayableAddressT -> "payable address"
  PrimitiveT BoolT -> "bool"
  PrimitiveT StringT -> "string"
  PrimitiveT (BytesT n) -> "bytes" <> showT n
  PrimitiveT (IntT n) -> "int" <> showT n
  PrimitiveT (UintT n) -> "uint" <> showT n
  PrimitiveT BytesDynamicT -> "bytes"
  PrimitiveT UnitT -> ""
  PrimitiveT (UserDefinedT id) -> id
  MappingT (MappingType k v) -> "mapping (" <> genType (PrimitiveT k) <> " => " <> genType v
  ArrayT (ArrayType t e) -> genType t <> "[" <> maybe "" genExpr e <> "]"

genVisibility :: Visibility -> SolText
genVisibility = \case
  Public -> "public"
  Private -> "private"
  Internal -> "internal"
  External -> "external"

genLocation :: MemoryLocation -> SolText
genLocation = \case
  Storage -> "storage"
  Memory -> "memory"
  Calldata -> "calldata"

genModifier :: Maybe StateVarModifier -> SolText
genModifier = \case
  Nothing -> ""
  Just Constant -> "constant"
  Just Immutable -> "immutable"

genMutability :: Mutability -> SolText
genMutability = \case
  Mutable -> ""
  View -> "view"
  Pure -> "pure"

genSpec :: SolText -> Bool -> SolText
genSpec txt spec = if spec then txt else ""

withIndent :: SolTextGen -> SolTextGen
withIndent f = do
    modify (& indentation +~ 1)
    r <- f
    modify (& indentation -~ 1)
    return r

withIndent' :: (a -> SolTextGen) -> [a] -> SolTextGen
withIndent' f as = do
    modify (& indentation +~ 1)
    r <- traverse f as
    modify (& indentation -~ 1)
    return $ T.concat r
