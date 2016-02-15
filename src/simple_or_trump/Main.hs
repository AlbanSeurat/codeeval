module Main where

import System.Environment (getArgs)
import Data.List (groupBy, elemIndex)
import Data.Function (on)

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


bestCard :: String -> String -> String
bestCard f s = maybe "ERROR" id $ elemIndex (init f) desk >>= (\fp -> elemIndex (init s) desk >>= (\sp -> if sp > fp then Just s else Just f))
        where desk = map show [2..10] ++ ["J", "Q", "K", "A"]

pickHigher :: String -> String -> String -> String
pickHigher (t:_) f s | last f == t && last s == t = pickHigher' f s
                 | last f == t = f
                 | last s == t = s
                 | otherwise = pickHigher' f s
                 where pickHigher' f s | init f == init s = f ++ " " ++ s
                                       | otherwise = bestCard f s

simpleOrTrump :: String -> String
simpleOrTrump x = pickHigher trump first second
        where [first, second, trump] = split " |" x


main = applyByLine simpleOrTrump
