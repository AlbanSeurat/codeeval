module Main where

import System.Environment

main :: IO()

main = do
    [fileName] <- getArgs
    content <- readFile fileName
    let linesOfFiles = lines content
    print linesOfFiles

