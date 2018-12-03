module Main where
import Data.List

contains :: Int -> String -> Bool
contains letterFrequency = any (== letterFrequency) . map length . group . sort

main :: IO ()
main = do
    input <- (readFile "../input")
    let ids = lines input
    let numIdsWithPairs   = length $ filter (contains 2) ids
    let numIdsWithTriples = length $ filter (contains 3) ids
    print (numIdsWithPairs * numIdsWithTriples)
