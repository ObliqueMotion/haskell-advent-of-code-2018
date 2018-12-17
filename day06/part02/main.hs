module Main where
import Data.List

type Point = (Int,Int)

-- Example: "152, 25" -> (152,25)
parsePoint :: String -> Point
parsePoint s = (x,y)
    where nums = words $ s \\ ","
          x = read (head nums) :: Int
          y = read (head $ tail nums) :: Int
        
-- Returns the samllest rectangle that contains a set of 2D points.
gridContainingAll :: [Point] -> [Point]
gridContainingAll points = [(x,y) | x <- [xMin..xMax], y <- [yMin..yMax]]
    where xMin = minimum xs
          xMax = maximum xs
          yMin = minimum ys
          yMax = maximum ys
          xs   = map fst points
          ys   = map snd points

-- Returns the Manhattan Distance between two 2D points.
distanceFrom :: Point -> Point -> Int
distanceFrom (x1,y1) (x2,y2) = (abs(x1-x2) + abs(y1-y2))

-- Returns the a set containing points that are closest to a target point.
-- If two or more points are equally close, they will all be in the set.
sumDistanceFromAll :: [Point] -> Point -> Int
sumDistanceFromAll points target = sum $ map (distanceFrom target) points

main :: IO ()
main = do
    input <- (readFile "../input")
    let points = map parsePoint $ lines input
    print $ length $ filter ((<10000) . (sumDistanceFromAll points)) $ gridContainingAll points
