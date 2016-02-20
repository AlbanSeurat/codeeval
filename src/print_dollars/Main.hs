module Main where

import System.Environment (getArgs)
import Data.List (groupBy, elemIndex)
import Data.Function (on)
import Data.Char (digitToInt)

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


printDollars x = printDollars' (length x - 1) x ++ "Dollars"
        where  units = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine" ]
               ties = ["", "Ten", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"]
               tens = ["", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen" ]
               printDollars' l ss@(x:xs) | l `mod` 3 == 2 = units !! pos ++ "Hundred" ++ printDollars' (l - 1) xs
                                         | l `mod` 3 == 1 = printTensDollars l ss
                                         | l `mod` 3 == 0 = units !! pos ++ printUnitsDollars l ss
                        where pos = digitToInt x
               printTensDollars l (x:xs) | x == '1' = tens !! nextPos ++ printUnitsDollars (l - 1) xs
                                         | otherwise = ties !! pos ++ printDollars' (l - 1) xs
                        where pos = digitToInt x
                              nextPos = digitToInt $ head xs
               printUnitsDollars l (x:xs) | l == 6 = "Million" ++ printDollars' 5 xs
                                          | l == 3 = "Thousand" ++ printDollars' 2 xs
                                          | l == 0 = ""

main = applyByLine printDollars
