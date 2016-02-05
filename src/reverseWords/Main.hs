module Main where

import System.Environment (getArgs)
import Data.List (intercalate)

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

reverseWords x = intercalate " " $ reverse $ words x

main = applyByLine reverseWords