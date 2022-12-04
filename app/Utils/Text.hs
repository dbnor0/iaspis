module Utils.Text where

import qualified Data.Text as T

showT :: (Show a) => a -> T.Text
showT = T.pack . show