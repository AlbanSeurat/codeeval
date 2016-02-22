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


inc 115 = 151
inc 151 = 511
inc 511 = 1015
inc 1015 = 1051


primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primeNumbers :: String -> String
primeNumbers max = intercalate "," $ map show $ takeWhile (< (read max)) $ primes


main = applyByLine primeNumbers
