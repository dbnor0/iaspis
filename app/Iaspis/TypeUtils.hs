{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Iaspis.TypeUtils where
  
import Iaspis.Grammar
import Data.Text as T
import Utils.Text


isNumeric :: Type -> Bool
isNumeric = \case
  UIntT -> True
  _ -> False

isBitwise :: Type -> Bool
isBitwise = \case
  BoolT -> False
  StringT -> False
  UnitT -> False
  _ -> True

isTruthy :: Type -> Bool
isTruthy = \case
  BoolT -> True
  _ -> False

numericUnaryOps :: [UnaryOp]
numericUnaryOps = [ArithmeticNegationOp, IncrementOp, DecrementOp]

arithOps :: [BinaryOp]
arithOps = [AdditionOp, SubtractionOp, MultiplicationOp, DivisionOp, ModuloOp]

eqOps :: [BinaryOp]
eqOps = [EqualityOp, InequalityOp]

logicalOps :: [BinaryOp]
logicalOps = [ConjunctionOp, DisjunctionOp]

relationalOps :: [BinaryOp]
relationalOps = [LessThanOp, GreaterThanOp, LessThanEqualOp, GreaterThanEqualOp]

bitwiseOps :: [BinaryOp]
bitwiseOps = [LeftShiftOp, RightShiftOp, BitwiseConjunctionOp, BitwiseDisjunctionOp, BitwiseExclDisjunctionOp]

name :: Type -> T.Text
name AddressT = "address"
name BoolT = "bool"
name (BytesT n) = "bytes" <> showT n
name UIntT = "uint"
name StringT = "string"
name UnitT = "unit"
name (UserDefinedT id) = id
name (StructT s) = structName s
name (EnumT e) = enumName e
name (ContractT id) = id



