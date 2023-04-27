{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingStrategies #-}

module Transpiler.LayoutCache.Layout where

import Transpiler.Iaspis.Grammar hiding (Type)
import Transpiler.Iaspis.TypeUtils
import Data.Maybe (mapMaybe)
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BS (unpack)
import Transpiler.LayoutCache.Types
import Transpiler.LayoutCache.Utils
import Control.Monad
import Data.Map hiding (mapMaybe)


persistLayout :: [Module] -> FilePath -> IO ()
persistLayout ms dir = do
  lc <- lastCache
  fp <- cacheFilename dir
  let layouts = serialize ms
  case lc of
    Nothing -> do
      writeFile fp (unpack $ encode layouts)
    Just lc' -> do
      let merged = elems $ mergeLayouts (toMap lc') (toMap layouts)
      when (or $ uncurry isUpdateInvalid <$> merged) 
        (error "Invalid memory layout upgrade")
      when (layouts /= lc') 
        (writeFile fp (unpack $ encode layouts))

serialize :: [Module] -> [MemoryLayout]
serialize ms = serializeModule =<< ms
  where serializeModule Module{ declarations } = mapMaybe serializeDecl declarations 
        serializeDecl (ProxyDecl p) = Just $ toLayout p
        serializeDecl _ = Nothing

toLayout :: ProxyContract -> MemoryLayout
toLayout ProxyContract{ proxyName, proxyDecls } = MemoryLayout proxyName (toDecl <$> proxyDecls) 

toDecl :: Field -> LayoutDecl
toDecl Field{ fieldName, fieldType } = LayoutDecl fieldName (name fieldType)