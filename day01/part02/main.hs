module Main where
import Data.HashSet
import qualified Data.List as L
 
parseInt :: String -> Int 
parseInt x = read $ (x L.\\ "+") :: Int 

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
