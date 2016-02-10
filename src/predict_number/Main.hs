module Main where

import System.Environment (getArgs)
import Data.List (groupBy, intercalate)
import Data.Function (on)
import Data.Char (intToDigit, digitToInt)
import Numeric (showHex, showIntAtBase)

main :: IO()

readInput :: IO [String]

readInput = do
            [fileName] <- getArgs
            content <- readFile fileName
            return $ lines content

applyByLine :: (String -> String) -> IO()
applyByLine func = fmap (map func) readInput >>= mapM_ putStrLn

split c x = filter (not . (==[c])) $ groupBy ((==) `on` (==c)) x

predictNumber :: Int -> Int
predictNumber 0 = 0
predictNumber x = (`mod` 3) . length . filter (=='1') $ showIntAtBase 2 intToDigit x ""

problemSolved x = show $ predictNumber $ read x

main = applyByLine problemSolved
