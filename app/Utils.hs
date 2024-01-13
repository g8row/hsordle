module Utils where

import System.Random (randomR, getStdRandom)
import Data.Map (Map)
import qualified Data.Map as Map

getRandomWord :: [String] -> IO String
getRandomWord words = do
  index <- getStdRandom (randomR (0, length words))
  return $ words !! index

validateInput :: String -> Int -> Bool
validateInput str len = length str == len && notElem ' ' str

printResult :: [Color] -> String
printResult = map toEmoji

createResult :: String -> String -> [Color]
createResult guess todaysWord = do
  let mapWithGreensAndLeftovers = foldl (\(map, left) (x,y) -> 
                                              if x == y 
                                                then ( Map.insert (length left + Map.size map) Green map, left) 
                                                else (map, y : left)) 
                                  (Map.empty, []) 
                                  (zip guess todaysWord)
  Map.elems $ fst $ foldl (\(map, left) (x,i) -> if  Map.lookup i map == Nothing
                                                  then if x `elem` left   
                                                        then (Map.insert i Yellow map, filter (/= x) left) 
                                                        else (Map.insert i Gray map, left)
                                                  else (map, left))
                          mapWithGreensAndLeftovers
                          (zip guess [0..])

readLenFromConsole :: IO Int
readLenFromConsole = do
  putStrLn ">> choose length of word: "
  line <- getLine
  let len = (read line :: Int)
  if len <= 0
    then do
      putStrLn ">> invalid input, choose length of word bigger than 0: "
      readLenFromConsole
    else if len >= 31
      then do
        putStrLn ">> invalid input, choose length of word smaller than 31: "
        readLenFromConsole
      else
        return len

chooseMode :: IO Mode
chooseMode = do
  putStrLn ">> choose a mode\n - 1. easy\n - 2. normal\n - 3. hard\n>> enter a number (1-3): "
  line <- getLine
  let mode = read line :: Int
  case mode of
    1 -> return Easy
    2 -> return Normal
    3 -> return Hard
    _ -> do
      putStrLn "\n>> invalid input, choose a number between 1 and 3"
      chooseMode

data Mode
  = Easy
  | Normal
  | Hard

data Color
  = Green
  | Yellow
  | Gray
  | Red
  deriving (Enum, Eq, Show)

toEmoji :: Color -> Char
toEmoji a = case a of
  Green -> '\129001' --'🟩'
  Yellow -> '\129000' --'🟨'
  Gray -> '⬜'
  Red -> '\128997' --'🟥'

fromEmoji :: Char -> Color
fromEmoji a = case a of
  '\129001' -> Green --'🟩'
  '\129000' -> Yellow --'🟨'
  '\11036' -> Gray -- '⬜'
  '\128997' -> Red --'🟥'

fromLetter :: Char -> Color
fromLetter a = case a of
  'g' -> Green --'🟩'
  'y' -> Yellow --'🟨'
  'w' -> Gray
  'r' -> Red --'🟥'


data Game = Wordle | Helper

chooseGame :: IO Game
chooseGame = do
  putStrLn ">> choose a mode\n - 1. Wordle\n - 2. Helper\n>> enter a number (1-2): "
  line <- getLine
  let mode = read line :: Int
  case mode of
    1 -> return Wordle
    2 -> return Helper
    _ -> do
      putStrLn "\n>> invalid input, choose a number between 1 and 2"
      chooseGame
