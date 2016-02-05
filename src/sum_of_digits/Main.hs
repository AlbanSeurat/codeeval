module Main where

import System.Environment (getArgs)
import Data.Char

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

sumDigits :: String -> String
sumDigits x = show $ sum [ digitToInt c | c <- x ]

main = applyByLine sumDigits