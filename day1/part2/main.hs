module Main where

import Data.HashSet

parseInt :: String -> Int
parseInt ('+':digits) = read digits :: Int
parseInt x = read x :: Int

firstReachedTwice :: [Int] -> Int
firstReachedTwice changeList = scan changeList (fromList [0]) 0
    where
    scan (change:changes) seen current
        | (newFrequency `member` seen) = newFrequency
        | otherwise = scan changes (insert newFrequency seen) newFrequency
        where newFrequency = current+change

main :: IO ()
main = do
    input <- (readFile "../input")
    let changeList = Prelude.map parseInt $ lines input
    print $ firstReachedTwice (cycle changeList)
