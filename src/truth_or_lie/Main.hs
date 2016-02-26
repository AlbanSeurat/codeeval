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


sumHex :: String -> Int
sumHex x = sum $ map readHex' $ split " " x
         where readHex' x = fst $ head $ readHex x

sumBinary :: String -> Int
sumBinary x = sum $ map readBin' $ split " " x
         where readBin' x = fst $ head $ readInt 2 (`elem` "01") digitToInt x

truthOrLie x = show $ sumHex left <= sumBinary right
            where [left, right] = split "|" x


main = applyByLine truthOrLie
