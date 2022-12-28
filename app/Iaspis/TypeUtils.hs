{-# LANGUAGE LambdaCase #-}

module Iaspis.TypeUtils where
  
import Iaspis.Grammar


isNumeric :: Type -> Bool
isNumeric = \case
  UIntT _ -> True
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
