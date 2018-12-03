module Main where
import Data.List
 
parseInt :: String -> Int
parseInt x = read $ (x \\ "+") :: Int

main :: IO ()
main = do
    input <- (readFile "../input")
    let changeList = lines input
    print $ sum $ map parseInt changeList
