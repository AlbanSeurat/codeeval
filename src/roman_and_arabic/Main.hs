module Main where

import System.Environment (getArgs)
import Data.List (groupBy, elemIndex, intercalate)
import Data.Function (on)
import Data.Char (digitToInt)
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


romainToArabic 'I' = 1
romainToArabic 'V' = 5
romainToArabic 'X' = 10
romainToArabic 'L' = 50
romainToArabic 'C' = 100
romainToArabic 'D' = 500
romainToArabic 'M' = 1000


tuples :: String -> [(Int, Int)]
tuples (x:y:xs) = (digitToInt x, romainToArabic y) : tuples xs
tuples [] = []

computeTuple :: (Int, Int) -> Int
computeTuple (x, y) = x * y

sumTuples (x:y:xs) | snd x > snd y = computeTuple x + sumTuples ((- fst y, snd y):xs)
             | otherwise = computeTuple x + sumTuples (y:xs)
sumTuples (x:[]) = computeTuple x
sumTuples [] = 0


aromatic x = show $ sumTuples $ reverse $ tuples x


main = applyByLine aromatic
