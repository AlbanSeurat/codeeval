module Main where

import System.Environment (getArgs)
import Data.List (intercalate, groupBy, permutations, sort)
import Data.Function (on)

main :: IO ()

readFileContent = do
           [fileName] <- getArgs
           content <- readFile fileName
           return $ lines content

problemSolved x = intercalate "," $ sort $ permutations x

main = fmap (unlines . map problemSolved) readFileContent >>= putStr
