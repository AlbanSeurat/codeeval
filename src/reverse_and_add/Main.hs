module Main where

import System.Environment (getArgs)
import Data.Char

main :: IO()

readInput :: IO [String]

readInput = do
            [fileName] <- getArgs
            content <- readFile fileName
            return $ lines content

applyByLine :: (String -> String) -> IO()
applyByLine func = fmap (map func) readInput >>= mapM_ putStrLn


sumReverse x = show $ (read x) + (read $ reverse x)

palindrome it x | x == reverse x = show it ++ " " ++ x
                | otherwise = palindrome (it + 1) $ sumReverse x


main = applyByLine $ palindrome 0

