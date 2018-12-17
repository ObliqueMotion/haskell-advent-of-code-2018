module Main where
import Data.List

hasRepeatedLetter :: Int -> String -> Bool
hasRepeatedLetter numReps = any (== numReps) . map length . group . sort

main :: IO ()
main = do
    input <- (readFile "../input")
    let ids = lines input
    let countIdsWithPairs   = length $ filter (hasRepeatedLetter 2) ids
    let countIdsWithTriples = length $ filter (hasRepeatedLetter 3) ids
    print (countIdsWithPairs * countIdsWithTriples)
