{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Options.Applicative
import CLI.Parser (commands)
import Control.Monad


main :: IO ()
main = join $ execParser (info (commands <**> helper) idm)