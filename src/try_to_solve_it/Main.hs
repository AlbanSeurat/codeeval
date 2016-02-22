module Main where

import System.Environment (getArgs)
import Data.List (groupBy, elemIndex, intercalate)
import Data.Function (on)
import Data.Char (digitToInt, intToDigit, ord, chr)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)
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


addC :: Char -> Int -> Char
addC c n = chr ((ord c) + n)

decode :: Char -> Char
decode c | c >= 'a' && c <= 'f' = addC c 20
         | c >= 'g' && c <= 'm' = addC c 7
         | c >= 'n' && c <= 't' = addC c (-7)
         | otherwise = addC c (-20)

tryToSolveIt x = map decode x

main = applyByLine tryToSolveIt
