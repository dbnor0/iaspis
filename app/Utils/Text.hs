module Utils.Text where

import Data.Text as T

showT :: (Show a) => a -> T.Text
showT = T.pack . show