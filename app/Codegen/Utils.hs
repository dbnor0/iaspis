{-# LANGUAGE OverloadedStrings #-}

module Codegen.Utils where

import  Data.Text as T
import Codegen.Types
import Control.Monad.State
import Lens.Micro.Platform
import Solidity.Grammar

indent :: Int -> T.Text
indent = flip T.replicate "  "

genText :: SolText -> State GenState SolText
genText t = do
  ind <- gets (^. indentation)
  return $ indent ind <> t

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