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


readBin :: String -> Maybe Int
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

grayToBin :: Int -> Int
grayToBin 0 = 0
grayToBin g = g `xor` (grayToBin $ g `shiftR` 1)

grayToBit :: String -> String
grayToBit x = maybe "error" show $ fmap grayToBin $ readBin x

beatOrBit x = intercalate " | " $ map grayToBit $ split " |" x


main = applyByLine beatOrBit
