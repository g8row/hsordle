{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}

module Main where

import System.IO (hSetEncoding, stdout, utf8)
import Utils
    ( Game(Wordle, Helper),
      Mode(Hard, Easy, Normal),
      getRandomWord,
      readLenFromConsole,
      chooseMode,
      chooseGame )
import Wordle ( playTurn, playTurnEasy, playTurnHard )
import Helper ( playHelper )
import Play (play)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  play
  


