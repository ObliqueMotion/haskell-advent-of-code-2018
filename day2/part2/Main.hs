module Main where

import Data.List (sort)

correctPair :: (String,String) -> Bool
correctPair (lhs,rhs) = 1 == (length $ filter (==True) $ zipWith (/=) lhs rhs)

sharedLetters :: (String,String) -> String
sharedLetters ([],[]) = []
sharedLetters ((x:xs),(y:ys))
    | x /= y = sharedLetters (xs,ys)
    | otherwise = x:(sharedLetters (xs,ys))

main :: IO ()
main = do
    input <- (readFile "files/input")
    let ids = sort $ lines input
    let idPairs = zip ids (tail ids)
    print $ map sharedLetters $ filter correctPair idPairs
