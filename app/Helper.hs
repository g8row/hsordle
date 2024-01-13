module Helper where

import Data.List (nub, intersect, (\\))
import System.Random (randomR, getStdRandom)
import Data.Map (Map)
import qualified Data.Map as Map
import Utils
    ( Color(Gray, Yellow, Green), createResult, fromLetter )
import Control.Lens ( (^?), element )

isPresentAt :: String -> Int -> Char -> Bool
isPresentAt word ind l = case word ^? element ind of
                              Nothing -> False
                              Just a -> a == l

getFilteredWords :: [String] -> [Char] -> [(Int, Char)] -> [(Int, Char)] -> Int -> [String]
getFilteredWords words grays yellows greens len = filter (\x -> length x == len 
                                                              && (intersect x grays) == []
                                                              && foldl (\acc (_,l) -> acc && l `elem` x) True yellows
                                                              && foldl (\acc (i,l) -> acc && not (isPresentAt x i l)) True yellows 
                                                              && foldl (\acc (i,l) -> acc && isPresentAt x i l) True greens) 
                                                       words

getRandomGuess :: [String] -> IO (Maybe String)
getRandomGuess [] = return Nothing
getRandomGuess filteredWords = do
      index <- getStdRandom (randomR (0, length filteredWords - 1))
      return $ filteredWords ^? element index

elimFilter :: [String] -> [String]
elimFilter filteredWords = 
    snd $ foldl (\(i, acc) x -> 
            let lenEliminated = length [word | word <- filteredWords, intersect word x /= []] in
              if lenEliminated > i 
                then (lenEliminated, [x]) 
                else if lenEliminated == i
                  then (i, x : acc)
                  else (i, acc)) 
          (0, []) 
          filteredWords

getMostEliminatingWord :: [String] -> IO (Maybe String)
getMostEliminatingWord [] = return Nothing
getMostEliminatingWord filteredWords = do
  index <- getStdRandom (randomR (0, length mostElims - 1))
  return $ mostElims ^? element index
  where mostElims = (elimFilter filteredWords)


interpretColors :: String -> [Color]
interpretColors = map fromLetter

playHelper :: String -> String -> [String] -> [Char] -> [(Int, Char)] -> [(Int, Char)] -> Int -> Int -> IO ()
playHelper _ _ _ _ _ _ _ 0 = putStrLn "u lose :/"
playHelper todaysWord prevGuess words grays yellows greens len turnsLeft = do
  putStrLn $ "answer: " ++ todaysWord
  putStrLn $ "guess:  " ++ prevGuess
  putStrLn $ ">> you have " ++ show turnsLeft ++ " turns left, input a list of colors according to the guess (w,y,g)"
  userInput <- getLine
  if length userInput /= len || interpretColors userInput /= createResult prevGuess todaysWord
    then do
      putStrLn ">> invalid input, try again"
      playHelper todaysWord prevGuess words grays yellows greens len turnsLeft
    else do
      let result = interpretColors userInput
      let yellows' = foldl (\acc r@(i, _) -> if result !! i == Yellow then acc ++ [r] else acc) yellows (zip [0..] prevGuess)
      let greens' = foldl (\acc r@(i, _) -> if result !! i == Green then acc ++ [r] else acc) greens (zip [0..] prevGuess)
      let grays' = foldl (\acc (x, i) -> if result !! i == Gray then acc ++ [x] else acc) grays (zip prevGuess [0..]) \\ (map snd yellows' ++ map snd greens')
      let filteredWords = getFilteredWords words grays' yellows' greens' len
      guess <- getMostEliminatingWord filteredWords
      case guess of
        Nothing -> do
          putStrLn ">> no words match your answer, try again"
          playHelper todaysWord prevGuess words grays yellows greens len turnsLeft
        Just guessStr -> do
          if guessStr == todaysWord 
            then putStrLn ">> we won woooo !!!"
            else do
              playHelper todaysWord guessStr (filter (/=guessStr) filteredWords) grays' yellows' greens' len (turnsLeft - 1) 
