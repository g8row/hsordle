module Helper where

import Data.List (nub, intersect, (\\))
import System.Random (randomR, getStdRandom)
import Data.Map (Map)
import qualified Data.Map as Map
import Utils
import Control.Lens

isPresentAt :: String -> Int -> Char -> Bool
isPresentAt word ind l = case word ^? element ind of
                              Nothing -> False
                              Just a -> a == l

getRandomGuess :: [String] -> [Char] -> [(Int, Char)] -> [(Int, Char)] -> Int -> IO (Maybe String)
getRandomGuess words grays yellows greens len = do
  let filteredWords = filter (\x -> length x == len 
                                    && (intersect x grays) == []
                                    && foldl (\acc (_,l) -> acc && l `elem` x) True yellows
                                    && foldl (\acc (i,l) -> acc && not (isPresentAt x i l)) True yellows 
                                    && foldl (\acc (i,l) -> acc && isPresentAt x i l) True greens) 
                              words
  if length filteredWords == 1
    then return $ Just $  head filteredWords
    else do
      index <- getStdRandom (randomR (0, length filteredWords - 1))
      return $ filteredWords ^? element index


interpretColors :: String -> [Color]
interpretColors = map fromLetter

playHelper :: String -> String -> [String] -> [Char] -> [(Int, Char)] -> [(Int, Char)] -> Int -> IO ()
playHelper todaysWord prevGuess words grays yellows greens len = do
  putStrLn $ "answer: " ++ todaysWord
  putStrLn $ "guess:  " ++ prevGuess
  putStrLn ">> input a list of colors according to the guess"
  userInput <- getLine
  if length userInput /= len
    then do
      putStrLn ">> invalid input, try again"
      playHelper todaysWord prevGuess words grays yellows greens len
    else do
      let result = interpretColors userInput
      let yellows' = foldl (\acc r@(i, _) -> if result !! i == Yellow then acc ++ [r] else acc) yellows (zip [0..] prevGuess)
      let greens' = foldl (\acc r@(i, _) -> if result !! i == Green then acc ++ [r] else acc) greens (zip [0..] prevGuess)
      let grays' = foldl (\acc (x, i) -> if result !! i == Gray then acc ++ [x] else acc) grays (zip prevGuess [0..]) \\ map snd yellows'
      print grays'
      print yellows'
      print greens'
      print prevGuess
      guess <- getRandomGuess words grays' yellows' greens' len
      case guess of
        Nothing -> do
          putStrLn ">> no words match your answer, try again"
          playHelper todaysWord prevGuess words grays yellows greens len
        Just guessStr -> do
          if guessStr == todaysWord 
            then putStrLn ">> we won woooo !!!"
            else do
              playHelper todaysWord guessStr words grays' yellows' greens' len 