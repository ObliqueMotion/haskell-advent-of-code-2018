module Main where
import Data.List

parsePoint :: String -> (Int,Int)
parsePoint s = (x,y)
    where nums = words $ s \\ ","
          x = read (head nums) :: Int
          y = read (head $ tail nums) :: Int
        
gridContainingAll :: [(Int,Int)] -> [(Int,Int)]
gridContainingAll points = [(x,y) | x <- [xMin..xMax], y <- [yMin..yMax]]
    where xMin = minimum xs
          xMax = maximum xs
          yMin = minimum ys
          yMax = maximum ys
          xs   = map fst points
          ys   = map snd points

distanceFrom :: (Int,Int) -> (Int,Int) -> Int
distanceFrom (x1,y1) (x2,y2) = (abs(x1-x2) + abs(y1-y2))

sumDistanceFromAll :: [(Int,Int)] -> (Int,Int) -> Int
sumDistanceFromAll points target = sum $ map (distanceFrom target) points

main :: IO ()
main = do
    input <- (readFile "../input")
    let points = map parsePoint $ lines input
    print $ length $ filter ((<10000) . (sumDistanceFromAll points)) $ gridContainingAll points
