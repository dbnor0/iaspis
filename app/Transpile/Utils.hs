{-# LANGUAGE OverloadedStrings #-}

module Transpile.Utils where

import Solidity.Grammar


uint :: Int -> Type
uint = PrimitiveT . UintT

bytes :: Int -> Type
bytes = PrimitiveT . BytesT

bytes' :: Type
bytes' = PrimitiveT BytesDynamicT

bool :: Type
bool = PrimitiveT BoolT

unit :: Type
unit = PrimitiveT UnitT

address :: Type
address = PrimitiveT AddressT

array :: Type -> Maybe Expression -> Type
array t s = ArrayT $ ArrayType t s

mapping :: PrimitiveType -> Type -> Type
mapping k v = MappingT $ MappingType k v

struct :: Identifier -> Type
struct = PrimitiveT . StructT

enum :: Identifier -> Type
enum = PrimitiveT . EnumT

contract :: Identifier -> Type
contract = PrimitiveT . ContractT

custom :: Identifier -> Type
custom = PrimitiveT . UserDefinedT