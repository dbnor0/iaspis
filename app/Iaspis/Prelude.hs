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
  }

preludeFields :: Bindings Field
preludeFields = M.empty

preludeFns :: Bindings FunctionHeader
preludeFns = M.empty