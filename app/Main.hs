{-# LANGUAGE UnicodeSyntax #-}

module Main where

import System.IO (hSetEncoding, stdout, utf8)
import System.Random (Random (randomR), getStdRandom)
import System.Win32.Console (setConsoleOutputCP)

member :: (Eq a) => a -> [a] -> Bool
member arg = foldr (\x acc -> acc || x == arg) False

validateInput :: String -> Int -> Bool
validateInput str len = length str == len && not (member ' ' str)

getTodaysWord :: [String] -> IO String
getTodaysWord words = do
  index <- getStdRandom (randomR (0, length words))
  return $ words !! index

playTurnEasy :: String -> [String] -> [Char] -> [Char] -> [Char] -> IO ()
playTurnEasy todaysWord words green yellow gray = do
  putStrLn $ "enter your guess, the length is " ++ show (length todaysWord)
  guess <- getLine
  let result = zipWith (\x y -> if x == y then '🟩' else if member x todaysWord then '🟨' else '⬜') guess todaysWord
  if validateInput guess $ length todaysWord
    then do
      let helpGreen = zipWith (\x y -> if y /= ' ' then if x == y then x else '-' else ' ') guess green
      let indexes = fst (foldl (\acc x -> if x == '-' then (fst acc ++ [show (snd acc)], snd acc + 1) else (fst acc, snd acc + 1)) ([], 0) helpGreen)
      if member '-' helpGreen
        then putStrLn $ "🟩 you already guessed letters at indexes " ++ foldl1 (\acc x -> (acc ++ [',']) ++ x) indexes
        else putStrLn "🟩 no hints for greens"
      if result == replicate (length todaysWord) '🟩'
        then do
          putStrLn result
          putStrLn "you win"
        else do
          putStrLn result
          playTurnEasy todaysWord words (zipWith3 (\x y z -> if x == y then x else if z == y then z else ' ') guess todaysWord green) yellow gray
    else do
      putStrLn $ "invalid input, try again, the length is " ++ show (length todaysWord)
      playTurnEasy todaysWord words (zipWith (\x y -> if x == y then x else ' ') guess todaysWord) yellow gray

main :: IO ()
main = do
  hSetEncoding stdout utf8
  setConsoleOutputCP 65001

  file <- readFile "app/words_alpha.txt"
  let words = lines file
  todaysWord <- getTodaysWord words

  putStrLn todaysWord
  playTurnEasy todaysWord words (replicate (length todaysWord) ' ') [] []