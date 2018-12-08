module Main where
import Data.Ord (comparing)
import Data.List
import Data.Function (on)
import qualified Data.HashSet as HS

type Point = (Int,Int)

-- Example: "152, 25" -> (152,25)
parsePoint :: String -> Point
parsePoint s = (x,y)
    where nums = words $ s \\ ","
          x = read (head nums) :: Int
          y = read (head $ tail nums) :: Int

-- Returns the smallest perimeter that encloses a set of 2D points.
-- Points will lie on the edge of the perimeter itself. 
perimeterAround :: [Point] -> [Point]
perimeterAround points = bottom ++ right ++ top ++ left ++ corners
    where corners = [(a,b) | a <- [xMin,xMax], b <- [yMin,yMax]]
          left    = [(a,b) | let a = (xMin), b <- [(yMin + 1)..(yMax - 1)]]
          right   = [(a,b) | let a = (xMax), b <- [(yMin + 1)..(yMax - 1)]]
          bottom  = [(a,b) | let b = (yMin), a <- [(xMin + 1)..(xMax - 1)]]
          top     = [(a,b) | let b = (yMax), a <- [(xMin + 1)..(xMax - 1)]]
          xMin    = minimum xs
          xMax    = maximum xs
          yMin    = minimum ys
          yMax    = maximum ys
          xs      = map fst points
          ys      = map snd points
        
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
closest :: [Point] -> Point -> [Point]
closest points target = head $ groupBy ((==) `on` (distanceFrom target)) $ sortBy (comparing (distanceFrom target)) points

main :: IO ()
main = do
    input <- (readFile "../input")
    let points = map parsePoint $ lines input
    let voronoi = concat $ filter ((==1) . length) $ map (closest points) $ gridContainingAll points
    let infinitePoints = HS.fromList $ nub $ concat $ filter ((==1) . length) $ map (closest points) $ perimeterAround points
    let finitePoints = filter (\p -> p `HS.notMember` infinitePoints) voronoi
    print $ maximum $ map length $ group $ sort finitePoints