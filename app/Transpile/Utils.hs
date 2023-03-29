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

struct :: Identifier -> Maybe MemoryLocation -> Type
struct id loc = PrimitiveT $ StructT id loc

enum :: Identifier -> Type
enum = PrimitiveT . EnumT

contract :: Identifier -> Type
contract = PrimitiveT . ContractT

custom :: Identifier -> Maybe MemoryLocation -> Type
custom id loc = PrimitiveT $ UserDefinedT id loc