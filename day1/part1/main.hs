module Main where
import Data.List

parseInt :: String -> Int
parseInt x = read $ (x \\ "+") :: Int

changeInFrequency :: [String] -> Int
changeInFrequency = sum . map parseInt

main :: IO ()
main = do
    input <- (readFile "../input")
    let changes = lines input
    print $ changeInFrequency changes
