{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Utils.Error where

import Data.Text as T

class IaspisError a where
  showErr :: a -> T.Text
