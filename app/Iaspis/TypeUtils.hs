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
  StringT _ -> False
  UnitT -> False
  _ -> True

isTruthy :: Type -> Bool
isTruthy = \case
  BoolT -> True
  _ -> False

isUserDefined :: Type -> Bool
isUserDefined (UserDefinedT _ _) = True
isUserDefined _ = False

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
name (StringT l) = "string " <> locName l
name UnitT = "unit"
name (UserDefinedT id l) = id <> " " <> locName l
name (StructT s l) = structName s <> " " <> locName l
name (EnumT e) = enumName e
name (ContractT id) = id

locName :: Maybe MemoryLocation -> T.Text
locName Nothing = ""
locName (Just Storage) = "storage"
locName (Just Memory) = "memory"

typeLoc :: Type -> Maybe MemoryLocation
typeLoc (StringT l) = l
typeLoc (StructT _ l) = l  
typeLoc (UserDefinedT _ l) = l 
typeLoc _ = Nothing

