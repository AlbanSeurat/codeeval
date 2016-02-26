module Main where

import System.Environment (getArgs)
import Data.List (groupBy, elemIndex, intercalate)
import Data.Function (on)
import Data.Char (isLetter, isSpace, digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readHex, readInt)
import Data.Bits

main :: IO()

readInput :: IO [String]

readInput = do
            [fileName] <- getArgs
            content <- readFile fileName
            return (lines content)

applyByLine :: (String -> String) -> IO()
applyByLine func = do
            lines <- readInput
            mapM_ putStrLn (map func lines)

split :: String -> String -> [String]
split s x = filter (not . (split' s)) $ groupBy ((==) `on` (`elem` s)) x
         where split' n xs = any (`elem` xs) n


stringToList x = map splitToList $ split ";" x
       where splitToList x = (f, s) where [f, s] = split "-" x

findNode :: String -> [(String, String)] -> [(String, String)]
findNode x xs = filter (\e -> fst e == x) xs
dropNode :: String -> [(String, String)] -> [(String, String)]
dropNode x xs = filter (\e -> not $ fst e == x) xs

chainFind :: String -> [(String, String)] -> [(String, String)]

chainFind x xs | f == [] = []
               | length xs == 1 = xs
               | otherwise = chainFind (snd $ head f) (dropNode x xs)
         where f = findNode x xs

isGoodChain x | x == [] = "BAD"
              | (snd $ head $ x) == "END" = "GOOD"
              | otherwise = "BAD"

chainInspection x = isGoodChain $ chainFind "BEGIN" $ stringToList x

main = applyByLine chainInspection
