module Main where

import Data.List

correctPair :: (String,String) -> Bool
correctPair (lhs,rhs) = 1 == (length $ filter (==True) $ zipWith (/=) lhs rhs)

sharedLetters :: (String,String) -> String
sharedLetters (lhs,rhs) = lhs \\ (lhs \\ rhs)

main :: IO ()
main = do
    input <- (readFile "files/input")
    let ids = sort $ lines input
    let idPairs = zip ids (tail ids)
    print $ map sharedLetters $ filter correctPair idPairs
