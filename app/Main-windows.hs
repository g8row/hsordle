{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}

module Main where

import System.IO (hSetEncoding, stdout, utf8)
import Data.Map (Map)
import qualified Data.Map as Map
import Utils
import Wordle
import Helper

maxTurns :: Int
maxTurns = 6

main :: IO ()
main = do
  hSetEncoding stdout utf8
  then System.Win32.Console.setConsoleOutputCP 65001
  
  file <- readFile "app/words_alpha.txt"
  let words = lines file

  len <- readLenFromConsole

  file <- readFile "app/words_alpha.txt"
  let words = lines file
  todaysWord <- getRandomWord words len
  firstGuess <- getRandomWord words len
  
  putStrLn todaysWord
  
  game <- chooseGame
  case game of
    Helper -> playHelper todaysWord firstGuess words [] [] [] len  
    Wordle -> do
      mode <- chooseMode
      case mode of
        Easy -> playTurnEasy todaysWord len words (replicate len ' ') [] [] 1 maxTurns
        Normal -> playTurn todaysWord len 1 maxTurns
        Hard -> playTurnHard todaysWord len Map.empty False 1 maxTurns


