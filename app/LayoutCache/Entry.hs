module LayoutCache.Entry where

import Transpiler.Iaspis.Grammar hiding (Type)
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BS (unpack)
import LayoutCache.Utils
import Control.Monad
import Data.Map hiding (mapMaybe)
import LayoutCache.Layout


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
