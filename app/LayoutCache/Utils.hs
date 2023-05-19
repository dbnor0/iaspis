module LayoutCache.Utils where

import Transpiler.Utils.File (getFilesWithExt)
import Data.Aeson
import Data.ByteString.Lazy.Char8 hiding (dropWhile, length, readFile, reverse)
import LayoutCache.Types
import Data.UnixTime (getUnixTime, UnixTime (utSeconds))
import System.FilePath
import Data.Maybe
import Text.Read
import Data.List
import Data.Map hiding (mapMaybe)


lastCache :: IO (Maybe [MemoryLayout])
lastCache = do
  fs <- getFilesWithExt ".meml" "./cache"
  let ts = mapMaybe ((readMaybe :: String -> Maybe Int) . dropExtension . takeFileName) fs
  case sortBy (flip compare) ts of
    [] -> return Nothing
    ts : _ -> do
      let fp = "./cache" </> show ts <> ".meml"
      l <- readFile fp
      return (decode (pack l) :: Maybe [MemoryLayout])

cacheFilename :: FilePath -> IO FilePath
cacheFilename dir = do
  f <- show . utSeconds <$> getUnixTime
  return $ dir </> f <> ".meml"

mergeLayouts :: (Ord k, Eq a) => Map k a -> Map k a -> Map k (a, a)
mergeLayouts l1 l2 = fromList merged
  where merged = [(k, (v1, v2)) | (k, v1) <- toList l1,
                                   let v2 = findWithDefault v1 k l2,
                                   v1 /= v2]

toMap :: [MemoryLayout] -> Map Id MemoryLayout
toMap mls = fromList $ toTuple <$> mls
  where toTuple ml@(MemoryLayout id _) = (id, ml)

isUpdateInvalid :: MemoryLayout -> MemoryLayout -> Bool
isUpdateInvalid (MemoryLayout _ l1) (MemoryLayout _ l2)
  | length l2 < length l1 = True
  | otherwise = go l1 l2
  where
    go [] [] = False
    go _ [] = True
    go [] _ = False
    go (x:xs) (y:ys)
      | x == y = go xs ys
      | otherwise = let restOfOriginal = xs
                        restOfModified = dropWhile (/= x) ys
                    in length restOfModified /= length restOfOriginal || restOfOriginal /= restOfModified
