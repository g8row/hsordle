module Play where

import Data.Map (Map)
import qualified Data.Map as Map
import Utils
    ( Game(Wordle, Helper),
      Mode(Hard, Easy, Normal),
      getMostEliminatingWord,
      getRandomWord,
      readLenFromConsole,
      chooseMode,
      chooseGame )
import Wordle ( playTurn, playTurnEasy, playTurnHard )
import Helper ( playHelper )

maxTurns :: Int
maxTurns = 6

play :: IO()
play = do
  len <- readLenFromConsole

  file <- readFile "app/words.txt"
  let words = filter (\x -> length x == len) (lines file)
  todaysWord <- getRandomWord words
    
  game <- chooseGame
  case game of
    Helper -> do
      firstGuess <- getMostEliminatingWord words
      let Just guess = firstGuess
      playHelper todaysWord guess words [] [] [] len maxTurns 
    Wordle -> do
      mode <- chooseMode
      case mode of
        Easy -> playTurnEasy todaysWord len words (replicate len ' ') [] [] 1 maxTurns
        Normal -> playTurn todaysWord len 1 maxTurns
        Hard -> playTurnHard todaysWord len Map.empty False 1 maxTurns