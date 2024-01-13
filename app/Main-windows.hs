{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}

module Main where

import System.IO (hSetEncoding, stdout, utf8)
import Play (play)

maxTurns :: Int
maxTurns = 6

main :: IO ()
main = do
  hSetEncoding stdout utf8
  System.Win32.Console.setConsoleOutputCP 65001
  play


