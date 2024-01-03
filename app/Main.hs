{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}

module Main where

import System.IO (hSetEncoding, stdout, utf8)
import System.Random (randomR, getStdRandom)
import System.Win32.Console (setConsoleOutputCP)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map

validateInput :: String -> Int -> Bool
validateInput str len = length str == len && notElem ' ' str

getTodaysWord :: [String] -> Int -> IO String
getTodaysWord words len = do
  let filteredWords = filter (\x -> length x == len) words
  index <- getStdRandom (randomR (0, length filteredWords))
  return $ filteredWords !! index

greenHints :: String -> String -> String
greenHints guess green = do
  let helpGreen = zipWith (\x y -> if y /= ' ' then if x == y then x else '-' else ' ') guess green
  let indexes = fst (foldl (\acc x -> if x == '-' then (fst acc ++ [show (snd acc)], snd acc + 1) else (fst acc, snd acc + 1)) ([], 0) helpGreen)
  if '-' `elem` helpGreen
    then
      if length indexes /= 1
        then toEmoji Green :" you already guessed the letters at indexes " ++ foldl1 (\acc x -> (acc ++ [',']) ++ x) indexes ++ "\n"
        else toEmoji Green :" you already guessed the letter at index " ++ head indexes ++ "\n"
    else ""

yellowHints :: String -> String -> String
yellowHints guess yellow = do
  let letters = foldl (\acc x -> if x `notElem` guess then if null acc then acc ++ [x] else acc ++ [',', x] else acc) [] yellow
  if not $ null letters
    then toEmoji Yellow :" these letters are known to appear: " ++ letters ++ "\n"
    else ""

grayHints :: String -> String -> String
grayHints guess gray = do
  let letters = foldl (\acc x -> if x `elem` guess then if null acc then acc ++ [x] else acc ++ [',', x] else acc) [] gray
  if not $ null letters
    then toEmoji Gray :" these letters are known to be useless: " ++ letters ++ "\n"
    else ""

dictHints :: String -> [String] -> String
dictHints guess words = if guess `notElem` words
  then toEmoji Red : " this word is not in the dictionary\n"
  else ""

printResult :: [Color] -> String
printResult = map toEmoji

createResult :: String -> String -> [Color]
createResult guess todaysWord = do
  let mapWithGreens = fst $ foldl (\(acc, ind) (x,y) -> if x == y then ( Map.insert ind (x, Green) acc, ind + 1) else (acc, ind + 1)) (Map.empty, 0) (zip guess todaysWord)
  let mapWithYellows = foldl (\acc (x,y) -> if x `elem` todaysWord
                                              && x `notElem` foldl (\acc (x,y) -> acc ++ [x]) [] acc
                                              && Map.lookup y mapWithGreens == Nothing
                                              then Map.insert y (x, Yellow) acc
                                              else acc)
                              mapWithGreens
                              (zip guess [0..])
  foldl
    ( \acc x ->
        case Map.lookup x mapWithYellows of
          Nothing -> acc ++ [Gray]
          Just a -> acc ++ [snd a]
    )
    []
    [0 .. (length todaysWord - 1)]

playTurnEasy :: String -> Int -> [String] -> String -> String -> String -> Int -> Int -> IO ()
playTurnEasy todaysWord len words green yellow gray currTurn maxTurn = do
  if currTurn - 1 == maxTurn
    then putStrLn "u lose :/"
    else do
      putStrLn ">> enter your guess:"
      guess <- getLine
      putStr "\n"
      let result = createResult guess todaysWord
      if validateInput guess len
        then do
          putStr $ dictHints guess words
          putStr $ greenHints guess green
          putStr $ yellowHints guess yellow
          putStr $ grayHints guess gray
          if result == replicate len Green
            then do
              putStrLn $ printResult result
              putStrLn "\nyou win!"
            else do
              putStrLn $ "\n" ++ printResult result ++ "\n" ++ foldl (\acc x -> acc ++ [x, ' ']) [] guess
              playTurnEasy
                todaysWord
                len
                words
                (zipWith3 (\x y z -> if x == y then x else if z == y then z else ' ') guess todaysWord green)
                (nub $ yellow ++ foldl (\acc x -> if x `elem` todaysWord then acc ++ [x] else acc) [] guess)
                (nub $ gray ++ foldl (\acc x -> if x `notElem` todaysWord then acc ++ [x] else acc) [] guess)
                (currTurn + 1) 
                maxTurn
        else do
          putStrLn $ ">> invalid input, try again, the length is " ++ show len
          playTurnEasy todaysWord len words green yellow gray (currTurn + 1) maxTurn

playTurn :: String -> Int -> Int -> Int -> IO ()
playTurn todaysWord len currTurn maxTurn = do
  if currTurn - 1 == maxTurn
    then putStrLn "u lose :/"
    else do
      putStrLn ">> enter your guess:"
      guess <- getLine
      putStr "\n"
      let result = createResult guess todaysWord
      if validateInput guess len
        then do
          if result == replicate len Green
            then do
              putStrLn $ printResult result
              putStrLn "\nyou win!"
            else do
              putStrLn $ "\n" ++ printResult result ++ "\n" ++ foldl (\acc x -> acc ++ [x, ' ']) [] guess
              playTurn todaysWord len (currTurn + 1) maxTurn
        else do
          putStrLn $ ">> invalid input, try again, the length is " ++ show len
          playTurn todaysWord len (currTurn + 1) maxTurn

lie :: Color -> IO Color
lie color = do
  rand <- getStdRandom (randomR (0, 2))
  let randColor = toEnum rand
  if randColor /= color
    then return randColor
    else lie color


lyingResult :: Map (Int, Char) Color -> String -> [Color] -> [IO Color]
lyingResult map guess result =
    foldl
      ( \acc (i, x) ->
          case Map.lookup (i, x) map of
            Nothing -> acc ++ [lie (result !! i)]
            Just a -> acc ++ [return a]
      )
      []
      (zip [0 ..] guess)

playTurnHard :: String -> Int -> Map (Int, Char) Color -> Bool -> Int -> Int -> IO ()
playTurnHard todaysWord len map haveLied currTurn maxTurn =
  if currTurn - 1 == maxTurn 
    then putStrLn "u lose :/"
    else do
      putStrLn ">> enter your guess:"
      guess <- getLine
      putStr "\n"
      let result = createResult guess todaysWord

      if validateInput guess len
        then do
          if result == replicate len Green
            then do
              putStrLn $ printResult result
              putStrLn "\nyou win!"

            else do
              rand <- getStdRandom (randomR (0, 2))
              if not haveLied && currTurn /= 1 && (rand :: Int) == 1
                then do
                      let resMonads = lyingResult map guess result
                      res <- sequence resMonads
                      --putStrLn "lying"
                      --print map
                      --putStrLn $ "\n" ++ printResult result ++ "\n" ++ foldl (\acc x -> acc ++ [x, ' ']) [] guess
                      putStrLn $ "\n" ++ printResult res ++ "\n" ++ foldl (\acc x -> acc ++ [x, ' ']) [] guess
                      playTurnHard todaysWord len Map.empty True (currTurn + 1) maxTurn
                    else do
                      putStrLn $ "\n" ++ printResult result ++ "\n" ++ foldl (\acc x -> acc ++ [x, ' ']) [] guess
                      playTurnHard
                        todaysWord
                        len
                        (foldl (\acc r@(i, x) -> Map.insert r (result !! i) acc) map (zip [0..] guess))
                        haveLied
                        (currTurn + 1)
                        maxTurn
                else do
                  putStrLn $ ">> invalid input, try again, the length is " ++ show len
                  playTurnHard todaysWord len map haveLied (currTurn + 1) maxTurn

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
  Green -> '🟩'
  Yellow -> '🟨'
  Gray -> '⬜'
  Red -> '🟥'

main :: IO ()
main = do
  hSetEncoding stdout utf8
  setConsoleOutputCP 65001

  let maxTurns = 6

  file <- readFile "app/words_alpha.txt"
  let words = lines file

  len <- readLenFromConsole

  file <- readFile "app/words_alpha.txt"
  let words = lines file
  todaysWord <- getTodaysWord words len

  mode <- chooseMode

  putStrLn todaysWord

  case mode of
    Easy -> playTurnEasy todaysWord len words (replicate len ' ') [] [] 1 maxTurns
    Normal -> playTurn todaysWord len 1 maxTurns
    Hard -> playTurnHard todaysWord len Map.empty False 1 maxTurns


