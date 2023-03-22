{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpile.Common where

import Iaspis.Grammar qualified as I
import Solidity.Grammar qualified as S


transpileFunctionDef :: I.Function -> S.FunctionDefinition
transpileFunctionDef I.Function{ I.functionHeader, I.functionBody } =
  S.FunctionDefinition
    { S.functionId = I.functionName functionHeader
    , S.functionVisibility = transpileVisibility . Just $ I.functionVisibility functionHeader
    , S.functionMutability = transpileMutability $ I.functionMutability functionHeader
    , S.functionPayablity = transpilePayability $ I.functionPayability functionHeader
    , S.functionVirtualSpec = False
    , S.functionOverrideSpec = False
    , S.functionArgs = transpileFunctionArg <$> I.functionArgs functionHeader
    , S.functionReturnType = [transpileFunctionArg $ I.functionReturnType functionHeader]
    , S.functionBody = transpileStmt <$> functionBody
    }

transpileFunctionArg :: I.FunctionArg -> S.FunctionArg
transpileFunctionArg I.FunctionArg{ I.argType, I.argLocation, I.argName } =
  S.FunctionArg
    { S.functionArgType = transpileType argType
    , S.functionArgLocation = transpileLocation argLocation
    , S.functionArgId = argName
    }

transpileDeclArg :: I.DeclArg -> S.FunctionArg
transpileDeclArg I.DeclArg{ I.declType, I.declLocation, I.declName } =
  S.FunctionArg
    { S.functionArgType = transpileType declType
    , S.functionArgLocation = transpileLocation declLocation
    , S.functionArgId = declName
    }

transpileStmt :: I.Statement -> S.Statement
transpileStmt = \case
  I.VarDeclStmt f _ e -> S.VarDeclStmt (transpileDeclArg f) (Just $ transpileExpr e)
  I.AssignmentStmt id _ e -> S.AssignmentStmt (transpileExpr id) (transpileExpr e)
  I.ReturnStmt e -> S.ReturnStmt $ transpileExpr <$> e
  I.IfStmt cond b1 b2 -> S.IfStmt (transpileExpr cond) (transpileStmt b1) (transpileStmt <$> b2)
  I.WhileStmt cond b -> S.WhileStmt (transpileExpr cond) (transpileStmt b)
  I.BlockStmt stmts -> S.BlockStmt $ transpileStmt <$> stmts
  I.BreakStmt -> S.BreakStmt
  I.ContinueStmt -> S.ContinueStmt
  I.ExpressionStmt e -> S.ExpressionStmt (transpileExpr e)

transpileExpr :: I.Expression -> S.Expression
transpileExpr = \case
  I.LiteralE v -> S.LiteralE $ transpileValue v
  I.IdentifierE id -> S.IdentifierE id
  I.MemberAccessE lv id -> S.MemberAccessE (transpileExpr lv) (transpileExpr $ I.IdentifierE id)
  I.FunctionCallE id es -> S.FunctionCallE (S.IdentifierE id) (transpileExpr <$> es)
  I.InstantiationE id es -> S.InstantiationE (S.IdentifierE id) (transpileExpr <$> es)
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

transpileReturnType :: I.Type -> S.FunctionArg
transpileReturnType t =
  S.FunctionArg
    { S.functionArgType = transpileType t
    , S.functionArgLocation = S.Memory
    , S.functionArgId = ""
    }

transpileType :: I.Type -> S.Type
transpileType = \case
  I.AddressT -> S.PrimitiveT S.AddressT
  I.BoolT -> S.PrimitiveT S.BoolT
  I.BytesT n -> S.PrimitiveT $ S.BytesT n
  I.UIntT -> S.PrimitiveT $ S.UintT 256
  I.StringT -> S.PrimitiveT S.StringT
  I.UnitT -> S.PrimitiveT S.UnitT
  I.UserDefinedT id -> S.PrimitiveT $ S.UserDefinedT id
  I.StructT s -> S.PrimitiveT $ S.StructT (I.structName s)
  I.EnumT e -> S.PrimitiveT $ S.EnumT (I.enumName e)
  I.ContractT id -> S.PrimitiveT $ S.ContractT id

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
  I.Payable -> True
  I.NonPayable -> False

transpileLocation :: I.MemoryLocation -> S.MemoryLocation
transpileLocation = \case
  I.Storage -> S.Storage
  I.Memory -> S.Memory

transpileValue :: I.Value -> S.Literal
transpileValue = \case
  I.AddressV v -> S.HexLit v
  I.BoolV b -> S.BooleanLit b
  I.BytesV v -> S.HexLit v
  I.UIntV n -> S.NumberLit n
  I.StringV v -> S.StringLit v
  -- TODO
  I.StructV s -> S.HexLit ""

