module Main where

parseInt :: String -> Int
parseInt ('+':digits) = read digits :: Int
parseInt x = read x :: Int

changeInFrequency :: [String] -> Int
changeInFrequency = foldl (+) 0 . map parseInt

main :: IO ()
main = do
    input <- (readFile "files/input")
    let changes = lines input
    print $ changeInFrequency changes
