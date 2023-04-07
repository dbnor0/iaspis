{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Transpiler.Iaspis.TypeUtils where

import Transpiler.Iaspis.Grammar
import Data.Text as T hiding (length)
import Transpiler.Utils.Text
import Prelude hiding (concatMap)


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

-- only used in variable declarations because
-- pointer declarations (storage variables) can only be initialized
-- to existing storage variables
strictEq :: Type -> Type -> Bool
strictEq AddressT AddressT = True
strictEq BoolT BoolT = True
strictEq (BytesT n1) (BytesT n2) = n1 >= n2
strictEq UIntT UIntT = True
strictEq UnitT UnitT = True
strictEq (EnumT e1) (EnumT e2) = e1 == e2
strictEq (UserDefinedT id1 l1) (UserDefinedT id2 l2) = id1 == id2 && l1 == l2
strictEq (StringT l1) (StringT l2) = l1 `strictLocEq` l2
strictEq (StructT s1 l1) (StructT s2 l2) = s1 == s2 && l1 `strictLocEq` l2 
strictEq (BytesDynT l1) (BytesDynT l2) = l1 `strictLocEq` l2
strictEq (BytesDynT _) (BytesT _) = True
strictEq (ArrayT t1 ds1 l1) (ArrayT t2 ds2 l2) = 
  t1 `strictEq` t2 
  && length ds1 == length ds2 
  && l1 `strictLocEq` l2
strictEq (ContractT id1) (ContractT id2) = id1 == id2
strictEq _ _ = False

strictLocEq :: Maybe MemoryLocation -> Maybe MemoryLocation -> Bool
strictLocEq Nothing _ = True
strictLocEq (Just Memory) _ = True
strictLocEq (Just Storage) (Just Storage) = True
strictLocEq _ _ = False

laxEq :: Type -> Type -> Bool
laxEq AddressT AddressT = True
laxEq BoolT BoolT = True
laxEq (BytesT n1) (BytesT n2) = n1 >= n2
laxEq UIntT UIntT = True
laxEq UnitT UnitT = True
laxEq (EnumT e1) (EnumT e2) = e1 == e2
laxEq (UserDefinedT id1 _) (UserDefinedT id2 _) = id1 == id2
laxEq (StringT _) (StringT _) = True
laxEq (StructT s1 _) (StructT s2 _) = s1 == s2
laxEq (BytesDynT _) (BytesDynT _) = True
laxEq (BytesDynT _) (BytesT _) = True
laxEq (ArrayT t1 ds1 _) (ArrayT t2 ds2 _) = 
  t1 `laxEq` t2 
  && length ds1 == length ds2
laxEq (ContractT id1) (ContractT id2) = id1 == id2
laxEq _ _ = False

name :: Type -> T.Text
name AddressT = "address"
name BoolT = "bool"
name (BytesT n) = "bytes" <> showT n
name UIntT = "uint"
name (StringT l) = "string " <> locName l
name (BytesDynT l) = "bytes " <> locName l
name UnitT = "unit"
name (UserDefinedT id l) = id <> " " <> locName l
name (StructT s l) = structName s <> " " <> locName l
name (ArrayT t ds l) = name t <> T.concat (nameD <$> ds) <> " " <> locName l
  where nameD Nothing = "[]"
        nameD (Just n) = "[" <> showT n <> "]"
name (MappingT t1 t2) = "mapping(" <> name t1 <> " => " <> name t2 <> ")"
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
typeLoc (ArrayT _ _ l) = l
typeLoc (BytesDynT l) = l
typeLoc (MappingT _ _) = Just Storage
typeLoc _ = Nothing

withLoc :: Type -> Maybe MemoryLocation -> Type
withLoc (StringT _) l = StringT l
withLoc (UserDefinedT id _) l = UserDefinedT id l
withLoc (StructT s _) l = StructT s l
withLoc (ArrayT t ds _) l = ArrayT t ds l
withLoc (BytesDynT _) l = BytesDynT l
withLoc t _ = t