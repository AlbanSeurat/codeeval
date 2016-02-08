module Main where

import System.Environment (getArgs)
import Data.List (intercalate)

main :: IO ()

readFileContent = do
           [fileName] <- getArgs
           content <- readFile fileName
           return $ lines content

parseLine :: String -> [Int]
parseLine xs = map read $ words xs

truncateLine (x:xs) = take x xs

isJolly xs = zipWith (\x y -> x /= 0 && abs (x - y) == 1) xs $ tail xs
showDifferences xs = zipWith (\x y -> abs (y - x)) xs $ tail xs

problemSolved x = if all (==True) (isJolly $ showDifferences $ truncateLine $ parseLine x) then "Jolly" else "Not jolly"

main = fmap (unlines . map problemSolved) readFileContent >>= putStr
