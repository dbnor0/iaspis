{-# LANGUAGE OverloadedStrings #-}

module Iaspis.Prelude where

import Analysis.Environment.Environment
import Data.Map as M
import Iaspis.Grammar


prelude :: Env
prelude = Env
  { _contracts = []
  , _proxies = []
  , _facets = []
  , _varEntries = preludeFields
  , _fnEntries = preludeFns
  , _typeEntries = preludeTypes
  }

preludeFields :: Bindings Field
preludeFields = M.empty

preludeFns :: Bindings FunctionHeader
preludeFns = M.empty

preludeTypes :: Bindings Type
preludeTypes = M.fromList 
  [ ("uint256", Entry "" (UIntT 256))
  , ("string", Entry "" StringT)
  , ("bool", Entry "" BoolT)
  , ("unit", Entry "" UnitT)
  , ("address", Entry "" AddressT)
  , ("bytes32", Entry "" (BytesT 32))
  , ("bytes", Entry "" BytesDynamicT)
  ]