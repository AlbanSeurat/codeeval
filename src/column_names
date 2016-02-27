module Main where

import System.Environment (getArgs)
import Data.List (groupBy, elemIndex, intercalate)
import Data.Function (on)
import Data.Char (chr, ord, toUpper)
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


intToString x = [chr (a + x)] where a = ord 'A'

column :: Int -> String
column x | x < 26 = intToString x
         | otherwise = column (x `div` 26 - 1) ++ intToString rest where rest = x `mod` 26

columnNames x = column $ (read x) - 1


main = applyByLine columnNames
