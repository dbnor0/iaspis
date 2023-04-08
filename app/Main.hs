{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Options.Applicative
import CLI.Parser (commands)
import Control.Monad
import System.IO


disableBuffering :: IO ()
disableBuffering = hSetBuffering stdout NoBuffering

main :: IO ()
main = do
  disableBuffering
  join $ execParser (info (commands <**> helper) idm)