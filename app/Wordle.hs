module Wordle where

import System.Random (randomR, getStdRandom)
import Data.List (nub, intersect, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Utils
    ( Color(..), validateInput, printResult, createResult, toEmoji )

greenHints :: String -> String -> String
greenHints guess green = do
  let helpGreen = zipWith (\x y -> if y /= ' ' then if x == y then x else '-' else ' ') guess green
  let indexes = fst (foldl (\acc x -> if x == '-' then (fst acc ++ [show (snd acc)], snd acc + 1) else (fst acc, snd acc + 1)) ([], 0) helpGreen)
  if '-' `elem` helpGreen
    then
      if length indexes /= 1
        then toEmoji Green :" you already guessed the letters at indexes " ++ foldl1 (\acc x -> (acc ++ [',']) ++ x) indexes ++ "\n"
        else let [x] = indexes in toEmoji Green :" you already guessed the letter at index " ++ x ++ "\n"
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

playTurnEasy :: String -> Int -> [String] -> [Char] -> [Char] -> [Char]  -> Int -> Int -> IO ()
playTurnEasy todaysWord len words green yellow gray currTurn maxTurn = do
  if currTurn - 1 == maxTurn
    then putStrLn $ todaysWord ++ " was the word, u lose :/"
    else do
      putStrLn $ ">> you are on turn " ++ show (maxTurn - currTurn + 1) ++ ", enter your guess:"
      guess <- getLine
      putStr "\n"
      let result = createResult guess todaysWord
      if validateInput guess len
        then do
          putStr $ dictHints guess words
          putStr $ greenHints guess green
          putStr $ yellowHints guess yellow
          putStr $ grayHints guess gray
          if guess == todaysWord
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
    then putStrLn $ todaysWord ++ " was the word, u lose :/"
    else do
      putStrLn $ ">> you are on turn " ++ show (maxTurn - currTurn + 1) ++ ", enter your guess:"
      guess <- getLine
      putStr "\n"
      let result = createResult guess todaysWord
      if validateInput guess len
        then do
          if guess == todaysWord
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


lyingResult :: Map (Int, Char) Color -> String -> [Color] -> IO [Color]
lyingResult map guess result = do
      let fakeGuess = sequence $ foldl
            ( \acc (i, x) ->
              case Map.lookup (i, x) map of
                Nothing -> case foldl (\acc (ind, a) -> case Map.lookup (ind, a) map of
                                                            Nothing -> acc
                                                            Just a -> if a == Gray then 2 else 1)
                                      3
                                      (zip [0..] (replicate (length guess) x)) of
                                1 -> acc ++ [return Yellow]
                                2 -> acc ++ [return Gray]
                                3 -> acc ++ [lie (result !! i)]
                Just a -> acc ++ [return a]
            )
            []
            (zip [0 ..] guess)
      deIod <- fakeGuess
      if deIod /= replicate (length guess) Green
        then fakeGuess
        else lyingResult map guess result

playTurnHard :: String -> Int -> Map (Int, Char) Color -> Bool -> Int -> Int -> IO ()
playTurnHard todaysWord len map haveLied currTurn maxTurn =
  if currTurn - 1 == maxTurn 
    then putStrLn $ todaysWord ++ " was the word, u lose :/"
    else do
      putStrLn $ ">> you are have " ++ show (maxTurn - currTurn + 1) ++ "turns left, enter your guess:"
      guess <- getLine
      putStr "\n"
      let result = createResult guess todaysWord

      if validateInput guess len
        then do
          if guess == todaysWord
            then do
              putStrLn $ printResult result
              putStrLn "\nyou win!"
            else do
              rand <- getStdRandom (randomR (0, 2))
              if not haveLied && currTurn /= 1 && (rand :: Int) == 1
                then do
                      res <- lyingResult map guess result
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
