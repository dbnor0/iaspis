{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}

module LayoutCache.Layout where

import Transpiler.Iaspis.Grammar hiding (Type)
import Transpiler.Iaspis.TypeUtils
import Data.Maybe (mapMaybe)
import LayoutCache.Types


serialize :: [Module] -> [MemoryLayout]
serialize ms = serializeModule =<< ms
  where serializeModule Module{ declarations } = mapMaybe serializeDecl declarations 
        serializeDecl (ProxyDecl p) = Just $ toLayout p
        serializeDecl _ = Nothing

toLayout :: ProxyContract -> MemoryLayout
toLayout ProxyContract{ proxyName, proxyDecls } = MemoryLayout proxyName (toDecl <$> proxyDecls) 

toDecl :: Field -> LayoutDecl
toDecl Field{ fieldName, fieldType } = LayoutDecl fieldName (name fieldType)