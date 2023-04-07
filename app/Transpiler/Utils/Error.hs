{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Transpiler.Utils.Error where

import Data.Text as T

class IaspisError a where
  showErr :: a -> T.Text
