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


calculate (x:xs) | x == "*"  = (calculate (init xs)) * (read $ last xs)
                 | x == "+"  = (calculate (init xs)) + (read $ last xs)
                 | x == "/"  = (calculate (init xs)) / (read $ last xs)
                 | otherwise = read x

prefixExpressions x = show $ round $ calculate $ words x

main = applyByLine prefixExpressions
