{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isNothing" #-}

module Main where

import Play (play)
import System.IO (hSetEncoding, stdout, utf8)
import System.Win32.Console (setConsoleOutputCP)

maxTurns :: Int
maxTurns = 6

main :: IO ()
main = do
  hSetEncoding stdout utf8
  setConsoleOutputCP 65001
  play
