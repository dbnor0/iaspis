{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen.Transpile where

import Iaspis.Grammar as I hiding (proxyDecls, facetDecls)
import Codegen.Types as S
import Solidity.Grammar as S
import Iaspis.DeclUtils
import Data.Maybe

transpile :: [I.Module] -> [S.Module]
transpile ms =
  let cs = contractDecls =<< ms
      ps = proxyDecls =<< ms
      fs = facetDecls =<< ms
  in catMaybes $ (transpileContract <$> cs) <> (transpileProxy <$> ps) <> (transpileFacet <$> fs)

transpileContract :: ([I.Identifier], I.Contract) -> Maybe S.Module
transpileContract (is, I.ImmutableContract { contractName, contractFields, contractFns }) =
  Just $ S.Module { moduleId=contractName, S.imports=is, decls=[ContractDef contractDef] }
  where contractDef =
          ContractDefinition
          { contractAbstractSpec = False
          , contractId = contractName
          , contractInheritanceList = []
          , contractBody = (StateVarDecl . transpileContractField <$> contractFields) <> (fns <$> contractFns)
          }
        fns f@(Function (FunctionHeader _ _ _ "constructor" _ _ _) _) = (ConstructorDef . transpileFunctionDef) f
        fns f@(Function (FunctionHeader _ _ _ "fallback" _ _ _) _) = (FallbackDef . transpileFunctionDef) f
        fns f@(Function (FunctionHeader _ _ _ "receive" _ _ _) _) = (ReceiveDef . transpileFunctionDef) f
        fns f = (FunctionDef . transpileFunctionDef) f


        
transpileContract _ = Nothing

transpileContractField :: I.Field -> S.StateVarDeclaration
transpileContractField Field{ fieldType, fieldVisibility, fieldName } =
  StateVarDeclaration
    { stateVarType = transpileType fieldType
    , stateVarVisibility = transpileVisibility fieldVisibility
    , stateVarModifier = Nothing
    , stateVarId = fieldName
    , stateVarInitializer = Nothing
    }

transpileFunctionDef :: I.Function -> S.FunctionDefinition 
transpileFunctionDef Function{ functionHeader, I.functionBody } =
  FunctionDefinition
    { S.functionId = functionName functionHeader
    , S.functionVisibility = transpileVisibility . Just $ I.functionVisibility functionHeader
    , S.functionMutability = transpileMutability $ I.functionMutability functionHeader
    , S.functionPayablity = transpilePayability $ I.functionPayability functionHeader
    , S.functionVirtualSpec = False
    , S.functionOverrideSpec = False
    , S.functionArgs = transpileFunctionArg <$> I.functionArgs functionHeader
    , S.functionReturnType = [transpileReturnType $ I.functionReturnType functionHeader]
    , S.functionBody = transpileStmt <$> functionBody
    }

transpileFunctionArg :: I.Field -> S.FunctionArg
transpileFunctionArg Field{fieldType, fieldLocation, fieldName} =
  FunctionArg 
    { functionArgType = transpileType fieldType
    , functionArgLocation = transpileLocation fieldLocation
    , functionArgId = fieldName
    }

transpileReturnType :: I.Type -> S.FunctionArg
transpileReturnType t =
  FunctionArg 
    { functionArgType = transpileType t
    , functionArgLocation = S.Memory
    , functionArgId = ""
    }

transpileStmt :: I.Statement -> S.Statement
transpileStmt = \case
  I.VarDeclStmt f ml _ -> S.VarDeclStmt (fieldName f) (transpileLocation ml) Nothing
  I.AssignmentStmt id _ e -> S.AssignmentStmt id (transpileExpr e)
  I.ReturnStmt e -> S.ReturnStmt $ transpileExpr <$> e
  I.IfStmt cond b1 b2 -> S.IfStmt (transpileExpr cond) (transpileStmt b1) (transpileStmt <$> b2)
  I.BlockStmt stmts -> S.BlockStmt $ transpileStmt <$> stmts
  I.BreakStmt -> S.BreakStmt
  I.ContinueStmt -> S.ContinueStmt
  I.ExpressionStmt e -> S.ExpressionStmt (transpileExpr e)

transpileExpr :: I.Expression -> S.Expression
transpileExpr = \case
  I.LiteralE v -> S.LiteralE $ transpileValue v
  I.IdentifierE id -> S.IdentifierE id
  I.FunctionCallE id es -> S.FunctionCallE (S.IdentifierE id) (transpileExpr <$> es)
  I.InstantiationE id es -> S.FunctionCallE (S.IdentifierE id) (transpileExpr <$> es)
  I.UnaryE op e -> S.UnaryE (transpileUnaryOp op) (transpileExpr e)
  I.BinaryE op e1 e2 -> S.BinaryE (transpileBinaryOp op) (transpileExpr e1) (transpileExpr e2)

transpileUnaryOp :: I.UnaryOp -> S.UnaryOp
transpileUnaryOp = \case
  I.ArithmeticNegationOp -> S.ArithmeticNegationOp
  I.LogicalNegationOp -> S.LogicalNegationOp
  I.BitwiseNegationOp -> S.BitwiseNegationOp
  I.IncrementOp -> S.IncrementOp
  I.DecrementOp -> S.DecrementOp

transpileBinaryOp :: I.BinaryOp -> S.BinaryOp
transpileBinaryOp = \case
  I.AdditionOp -> S.AdditionOp
  I.SubtractionOp -> S.SubtractionOp
  I.MultiplicationOp -> S.MultiplicationOp
  I.DivisionOp -> S.DivisionOp
  I.ModuloOp -> S.ModuloOp
  I.ConjunctionOp -> S.ConjunctionOp
  I.DisjunctionOp -> S.DisjunctionOp
  I.EqualityOp -> S.EqualityOp
  I.InequalityOp -> S.InequalityOp
  I.LessThanOp -> S.LessThanOp
  I.GreaterThanOp -> S.GreaterThanOp
  I.LessThanEqualOp -> S.LessThanEqualOp
  I.GreaterThanEqualOp -> S.GreaterThanEqualOp
  I.LeftShiftOp -> S.LeftShiftOp
  I.RightShiftOp -> S.RightShiftOp
  I.BitwiseConjunctionOp -> S.BitwiseConjunctionOp
  I.BitwiseDisjunctionOp -> S.BitwiseDisjunctionOp
  I.BitwiseExclDisjunctionOp -> S.BitwiseExclDisjunctionOp

transpileProxy :: ([I.Identifier], I.Contract) -> Maybe S.Module
transpileProxy (is, I.ProxyContract { proxyName, facetList }) =
  Just $ S.Module { moduleId=proxyName, S.imports=is <> facetList, decls=[] }
transpileProxy _ = Nothing

transpileFacet :: ([I.Identifier], I.Contract) -> Maybe S.Module
transpileFacet (is, I.FacetContract { facetName, proxyList }) =
  Just $ S.Module { moduleId=facetName, S.imports=is <> [proxyList], decls=[] }
transpileFacet _ = Nothing

transpileType :: I.Type -> S.Type
transpileType = \case
  I.AddressT -> S.PrimitiveT S.AddressT
  I.BoolT -> S.PrimitiveT S.BoolT
  I.BytesT n -> S.PrimitiveT $ S.BytesT n
  I.BytesDynamicT -> S.PrimitiveT S.BytesDynamicT
  I.UIntT n -> S.PrimitiveT $ S.UintT n
  I.StringT -> S.PrimitiveT S.StringT
  I.UnitT -> S.PrimitiveT S.UnitT
  I.UserDefinedT id -> S.PrimitiveT $ S.UserDefinedT id

transpileVisibility :: Maybe I.MemberVisibility -> S.Visibility
transpileVisibility = \case
  Just I.Public -> S.Public
  Just I.Internal -> S.Internal
  Just I.External -> S.External
  _ -> S.Private

transpileMutability :: I.Mutability -> S.Mutability
transpileMutability = \case
  I.Mutable -> S.Mutable
  I.View -> S.View

transpilePayability :: I.PayabilityKind -> Bool
transpilePayability = \case
  Payable -> True
  NonPayable -> False

transpileLocation :: I.MemoryLocation -> S.MemoryLocation
transpileLocation = \case
  I.Storage -> S.Storage
  I.Memory -> S.Memory

transpileValue :: I.Value -> S.Literal
transpileValue = \case  
  AddressV v -> S.HexLit v
  BoolV b -> S.BooleanLit b
  BytesV v -> S.HexLit v
  UIntV n -> S.NumberLit n
  StringV v -> S.StringLit v
