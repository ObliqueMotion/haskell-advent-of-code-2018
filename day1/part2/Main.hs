module Main where

import Data.HashSet

parseInt :: String -> Int
parseInt ('+':digits) = read digits :: Int
parseInt x = read x :: Int

firstReachedTwice :: (Maybe Int, HashSet Int, Int) -> [Int] -> Int
firstReachedTwice (Just answer,_,_) _ = answer
firstReachedTwice (Nothing,seen,current) changeList = firstReachedTwice (scan changeList seen current) changeList
    where 
    scan [] seen current = (Nothing, seen, current)
    scan (change:changeList) seen current
        | (member newFrequency seen) = (Just newFrequency,seen,current)
        | otherwise = scan changeList (insert newFrequency seen) newFrequency
        where newFrequency = current+change

main :: IO ()
main = do
    input <- (readFile "files/input")
    let changeList = Prelude.map parseInt $ lines input
    let hashSet = fromList [0]
    print $ firstReachedTwice (Nothing, hashSet, 0) changeList
