{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Transpiler.Codegen.Generate where

import Transpiler.Codegen.Types
import Data.Text as T
import Transpiler.Solidity.Grammar as S
import Data.Text.IO as T
import Control.Monad.State ( State, modify, evalState )
import Data.List qualified as L
import Data.Function
import Lens.Micro.Platform
import Transpiler.Yul.Grammar as Y
import Transpiler.Utils.Text
import Transpiler.Transpile.Types qualified as T
import Data.List qualified


type SolTextGen = State GenState SolText

genFile :: FilePath -> (FilePath, SolText) -> IO ()
genFile dir (name, code) = T.writeFile (dir <> "\\" <> name <> ".sol") code

genModule :: T.Module -> (FilePath, SolText)
genModule T.Module{ T.moduleId, T.imports, T.decls } = (T.unpack moduleId, code)
  where code = evalState genModule' (GenState 0)
        genModule' = do
          l <- genLicense
          is <- traverse genImport imports
          ds <- traverse genDecl decls
          return $ l <> T.concat is <> T.concat ds

genLicense :: SolTextGen
genLicense = return "// SPDX-License-Identifier: MIT\n"

genImport :: S.Identifier -> SolTextGen
genImport i = return $ "import \"./" <> i <> ".sol\";\n"

genDecl :: T.Declaration -> SolTextGen
genDecl = \case
  T.ContractDef cd -> genContract cd
  T.InterfaceDef id -> genInterface id
  T.LibraryDef ld -> genLibrary ld
  T.StructTypeDef sd -> genStruct sd
  T.EnumDef ed -> genEnum ed

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
  S.StateVarDecl svd -> genStateVarDecl svd
  S.ConstructorDef f -> genFunction f
  S.FunctionDef f -> genFunction f
  S.FallbackDef f -> genFunction f
  S.ReceiveDef f -> genFunction f
  S.StructDef s -> genStruct s
  S.EnumDef e -> genEnum e

genStateVarDecl :: StateVarDeclaration -> SolTextGen
genStateVarDecl (StateVarDeclaration t v m id e) = genText decl
  where decl = genPlainType t <> " " <> genVisibility v <> " " <> genModifier m <> " " <> id <> init e <> ";\n"
        init = maybe "" (\e -> " = " <> genExpr e)

genFunction :: FunctionDefinition -> SolTextGen
genFunction f@FunctionDefinition{ functionBody } = do
  h <- genFunctionHeader f
  b <- withIndent' genStmt functionBody
  cb <- genText "}\n\n"
  if Data.List.null functionBody then
    return $ h <> ";\n\n"
  else
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
          | fn `Prelude.elem` ["constructor", "receive", "fallback"] = ""
          | otherwise = "function "

genFunctionArg :: FunctionArg -> SolText
genFunctionArg (FunctionArg t id) = genType t <> " " <> id

genReturnType :: [FunctionArg] -> SolText
genReturnType [FunctionArg (PrimitiveT UnitT) _] = ""
genReturnType [a] = "returns (" <> genFunctionArg a <> ")"
genReturnType args = "returns (" <> T.concat (L.intersperse "," (genFunctionArg <$> args)) <> ")"

genStmt :: S.Statement -> SolTextGen
genStmt = \case
  S.BlockStmt stmts -> do
    ob <- genText "{\n"
    ss <- withIndent' genStmt stmts
    cb <- genText "}\n"
    return $ ob <> ss <> cb
  S.VarDeclStmt f e -> do
    let decl = genType (functionArgType f) <> " " <>  functionArgId f
        expr = maybe "" (\e -> " = " <> genExpr e) e
    genText $ decl <> expr <> ";\n"
  S.AssignmentStmt id e -> genText $ genExpr id <> " = " <> genExpr e <> ";\n"
  S.ExpressionStmt e -> genText $ genExpr e <> ";\n"
  S.IfStmt cond b1 b2 -> do
    c <- genText $ "if(" <> genExpr cond <> ") "
    b1' <- withIndent $ genStmt b1
    b2' <- maybe (return "") genElse b2
    return $ c <> b1' <> b2'
    where genElse s = do
            e <- genText "else "
            b <- withIndent $ genStmt s
            genText $ e <> b
  S.ForStmt s c i b -> do
    s' <- genStmt s
    c' <- genStmt c
    let i' = genExpr i
    b' <- withIndent $ genStmt b
    return $ "for(" <> s' <> " " <> c' <> " " <> i' <> ")" <> b'
  S.WhileStmt cond b -> do
    c <- genText $ "while(" <> genExpr cond <> ") {"
    ss <- withIndent $ genStmt b
    cb <- genText "}"
    return $ c <> ss <> cb
  S.ContinueStmt -> genText "continue;\n"
  S.BreakStmt -> genText "break;\n"
  S.ReturnStmt e -> genText $ "return " <> maybe "" genExpr e <> ";\n"
  S.RevertStmt e -> genText $ "revert(" <> genExpr e <> ");\n"
  S.AssemblyStmt asm -> do
    s <- genText "assembly {\n"
    asm' <- withIndent $ genYulStmt asm
    e <- genText "}\n"
    return $ s <> asm' <> e
  S.NoOpStmt -> return ""

genYulStmt :: Y.Statement -> SolTextGen
genYulStmt = \case
  Y.BlockStmt stmts -> do
    s <- genText "{\n"
    ss <- withIndent' genYulStmt stmts
    e <- genText "}\n"
    return $ s <> ss <> e
  Y.VarDeclStmt id e -> do
    genText $ "let " <> id <> " := " <> genYulExpr e  <> "\n"
  Y.AssignmentStmt lv e -> do
    genText $ genYulExpr lv <> " := " <> genYulExpr e <> "\n"
  Y.IfStmt c b -> do
    c' <- genText $ "if " <> genYulExpr c <> " {\n"
    b' <- withIndent $ genYulStmt b
    e <- genText "}\n"
    return $ c' <> b' <> e
  Y.ExpressionStmt e -> do
    e' <- withIndent $ pure $ genYulExpr e
    genText $ e' <> "\n"
  Y.SwitchStmt cond bs -> do
    s <- genText $ "switch " <> genYulExpr cond <> "\n"
    bs' <- withIndent' genYulSwitchCase bs
    return $ s <> bs'

genYulSwitchCase :: (Y.Identifier, Y.Statement) -> SolTextGen
genYulSwitchCase ("", s) = do
  s' <- genText "default {\n"
  b <- withIndent $ genYulStmt s
  e <- genText "}\n"
  return $ s' <> b <> e
genYulSwitchCase (c, s) = do
  s' <- genText $ "case " <> c <> " {\n"
  b <- withIndent $ genYulStmt s
  e <- genText "}\n"
  return $ s' <> b <> e


genYulExpr :: Y.Expression -> SolText
genYulExpr = \case
  Y.IdentifierE id -> id
  Y.BuiltinE b -> genYulBuiltin b
  Y.PathE lv m -> genYulExpr lv <> "." <> genYulExpr m
  Y.LiteralE l -> genYulLit l
  Y.FunctionCallE id es -> genYulExpr id <> "(" <> T.concat (Data.List.intersperse "," (genYulExpr <$> es)) <> ")"

genYulBuiltin :: Y.Builtin -> SolText
genYulBuiltin = T.toLower . showT

genExpr :: S.Expression -> SolText
genExpr = \case
  S.LiteralE lit -> genLit lit
  S.IdentifierE id -> id
  S.InlineArrayE es -> "[" <> T.concat (L.intersperse "," (genExpr <$> es)) <> "]"
  S.MemberAccessE lv m -> genExpr lv <> "." <> genExpr m
  S.SubscriptE lv i -> genExpr lv <> "[" <> genExpr i <> "]"
  S.FunctionCallE id args -> genFunctionCallExpr id args
  S.InstantiationE id args -> "new " <> genExpr id <> "(" <> T.concat (L.intersperse "," (genExpr <$> args)) <> ")"
  S.ArrayInstantiationE id size -> "new " <> id <> "[](" <> genExpr size <> ")"
  S.CastE t e -> genType t <> "(" <> genExpr e <> ")"
  S.BinaryE op e1 e2 -> genExpr e1 <> " " <> genBinaryOp op <> " " <> genExpr e2
  S.UnaryE op e -> genUnaryOp op <> " " <> genExpr e

genFunctionCallExpr :: S.Expression -> [S.Expression] -> SolText
genFunctionCallExpr (S.IdentifierE "sconcat") [e1, e2] = 
  "string.concat(" <> genExpr e1 <> ", " <> genExpr e2 <> ")"
genFunctionCallExpr (S.IdentifierE "bconcat") [e1, e2] = 
  "bytes.concat(" <> genExpr e1 <> ", " <> genExpr e2 <> ")"
genFunctionCallExpr (S.IdentifierE "balanceof") [e] = 
  genExpr e <> ".balance"
genFunctionCallExpr (S.IdentifierE "transfer") [e1, e2] = 
  genExpr e1 <> ".transfer(" <> genExpr e2 <> ")"
genFunctionCallExpr (S.IdentifierE "send") [e1, e2] = 
  genExpr e1 <> ".send(" <> genExpr e2 <> ")"
genFunctionCallExpr (S.IdentifierE "call") [e1, e2] = 
  genExpr e1 <> ".call(" <> genExpr e2 <> ")"
genFunctionCallExpr (S.IdentifierE "delegatecall") [e1, e2] = 
  genExpr e1 <> ".delegatecall(" <> genExpr e2 <> ")"
genFunctionCallExpr (S.IdentifierE "staticcall") [e1, e2] = 
  genExpr e1 <> ".staticcall(" <> genExpr e2 <> ")"
genFunctionCallExpr id args = genExpr id <> "(" <> T.concat (L.intersperse "," (genExpr <$> args)) <> ")"

genLit :: S.Literal -> SolText
genLit = \case
  S.StringLit v -> "\"" <> v <> "\""
  S.NumberLit n -> showT n
  S.BooleanLit b -> T.toLower $ showT b
  S.HexLit v -> "0x" <> v
  S.EnumLit e v -> e <> "." <> v
  S.StructLit id ms -> id <> "({" <> T.concat (L.intersperse "," (genStructLitMember <$> ms)) <> "})"
  S.ArrayLit es -> "[" <> T.concat (L.intersperse "," (genExpr <$> es)) <> "]"

genStructLitMember :: (S.Identifier, S.Expression) -> SolText
genStructLitMember (id, e) = id <> ": " <> genExpr e

genYulLit :: Y.Literal -> SolText
genYulLit = \case
  Y.StringLit v -> "\"" <> v <> "\""
  Y.NumberLit n -> showT n
  Y.BooleanLit b -> T.toLower $ showT b
  Y.HexLit v -> v

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
genInterface (InterfaceDefinition id inhL b) = do
  let cs = T.concat $ L.intersperse ", " inhL
      inhCs = if not $ Prelude.null inhL then " is " <> cs else ""
      header = "interface " <> id <> inhCs <> "{\n"
  es <- withIndent' genContractBodyElem b
  return $ header <> es <> "}\n"

genLibrary :: LibraryDefinition -> SolTextGen
genLibrary LibraryDefinition { libraryId, libraryBody } = do
  es <- withIndent' genContractBodyElem libraryBody
  genText $ "library " <> libraryId <> " {\n" <> es <> "}\n"

genStruct :: StructDefinition -> SolTextGen
genStruct StructDefinition{ structId, structMembers } = do
  ms <- withIndent' genStructMember structMembers
  cb <- genText "}\n"
  genText $ "struct " <> structId <> " {\n" <> ms <> cb

genStructMember :: StructMember -> SolTextGen
genStructMember (StructMember t id) = do
  genText $ genPlainType t <> " " <> id <> ";\n"

genEnum :: EnumDefinition -> SolTextGen
genEnum EnumDefinition{ enumId, enumMembers } = do
  modify (& indentation +~ 1)
  ms <- traverse genText enumMembers
  modify (& indentation -~ 1)
  cb <- genText "}\n"
  genText $ "enum " <> enumId <> " {\n" <> T.concat (Data.List.intersperse ",\n" ms) <> "\n" <> cb

genEnumMember :: S.Identifier -> SolTextGen
genEnumMember id = genText $ id <> ",\n"

genPlainType :: Type -> SolText
genPlainType = \case
  PrimitiveT AddressT -> "address"
  PrimitiveT PayableAddressT -> "payable address"
  PrimitiveT BoolT -> "bool"
  PrimitiveT (StringT _) -> "string"
  PrimitiveT (BytesT n) -> "bytes" <> showT n
  PrimitiveT (IntT n) -> "int" <> showT n
  PrimitiveT (UintT n) -> "uint" <> showT n
  PrimitiveT (BytesDynamicT _) -> "bytes"
  PrimitiveT UnitT -> ""
  PrimitiveT (UserDefinedT id _) -> id
  PrimitiveT (StructT id _) -> id
  PrimitiveT (EnumT id) -> id
  PrimitiveT (ContractT id) -> id
  MappingT (MappingType k v) -> "mapping (" <> genType k <> " => " <> genType v <> ")"
  ArrayT (ArrayType t ds _) -> genType t <> T.concat (genDim <$> ds)
  where genDim Nothing = "[]"
        genDim (Just n) = "[" <> showT n <> "]"


genType :: Type -> SolText
genType = \case
  PrimitiveT AddressT -> "address"
  PrimitiveT PayableAddressT -> "payable address"
  PrimitiveT BoolT -> "bool"
  PrimitiveT (StringT l) -> "string " <> genLocation l
  PrimitiveT (BytesT n) -> "bytes" <> showT n
  PrimitiveT (IntT n) -> "int" <> showT n
  PrimitiveT (UintT n) -> "uint" <> showT n
  PrimitiveT (BytesDynamicT l) -> "bytes " <> genLocation l
  PrimitiveT UnitT -> ""
  PrimitiveT (UserDefinedT id l) -> id <> " " <> genLocation l
  PrimitiveT (StructT id l) -> id <> " " <> genLocation l
  PrimitiveT (EnumT id) -> id
  PrimitiveT (ContractT id) -> id
  MappingT (MappingType k v) -> "mapping (" <> genPlainType k <> " => " <> genPlainType v <> ")"
  ArrayT (ArrayType t ds l) -> genPlainType t <> T.concat (genDim <$> ds) <> " " <> genLocation l
  where genDim Nothing = "[]"
        genDim (Just n) = "[" <> showT n <> "]"

genVisibility :: Visibility -> SolText
genVisibility = \case
  Public -> "public"
  Private -> "private"
  Internal -> "internal"
  External -> "external"

genLocationWithType :: Type -> MemoryLocation -> SolText
genLocationWithType t l =
  case (t, l) of
    (ArrayT _, l) -> genLocation $ Just l
    (MappingT _, l) -> genLocation $ Just l
    (PrimitiveT (BytesDynamicT l), _) -> genLocation l
    (PrimitiveT (StringT l), _) -> genLocation l
    (PrimitiveT (StructT _ l), _) -> genLocation l
    (PrimitiveT (ContractT _), l) -> genLocation $ Just l
    _ -> ""

genLocation :: Maybe MemoryLocation -> SolText
genLocation = \case
  Nothing -> ""
  Just Storage -> "storage"
  Just Memory -> "memory"
  Just Calldata -> "calldata"

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

