{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
module Transpiler.LayoutCache.Cache where

import Transpiler.Iaspis.Grammar hiding (Type)
import  Data.Text qualified as T
import Transpiler.Iaspis.TypeUtils
import Data.Maybe (mapMaybe)

type Id = T.Text
type Type = T.Text

data LayoutDecl = LayoutDecl Id Type

data MemoryLayout = MemoryLayout Id [LayoutDecl]

serialize :: [Module] -> [MemoryLayout]
serialize ms = serializeModule =<< ms
    where serializeModule Module{ declarations } = mapMaybe serializeDecl declarations 
          serializeDecl (ProxyDecl p) = Just $ toLayout p
          serializeDecl _ = Nothing

toLayout :: ProxyContract -> MemoryLayout
toLayout ProxyContract{ proxyName, proxyDecls } = MemoryLayout proxyName (toDecl <$> proxyDecls) 

toDecl :: Field -> LayoutDecl
toDecl Field{ fieldName, fieldType } = LayoutDecl fieldName (name fieldType)