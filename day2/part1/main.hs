module Main where

import Data.List

contains :: Int -> String -> Bool
contains frequency = any (== frequency) . map length . group . sort

main :: IO ()
main = do
    input <- (readFile "../input")
    let ids = lines input
    let pair = 2
    let triple = 3
    print $ (length $ filter (contains pair) ids) * (length $ filter (contains triple) ids)
