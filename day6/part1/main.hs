module Main where
import Data.Ord (comparing)
import Data.List
import Data.Function (on)
import qualified Data.HashSet as HS

parsePoint :: String -> (Int,Int)
parsePoint s = (x,y)
    where nums = words $ s \\ ","
          x = read (head nums) :: Int
          y = read (head $ tail nums) :: Int

perimiterAround :: [(Int,Int)] -> [(Int,Int)]
perimiterAround points = bottom ++ right ++ top ++ left ++ corners
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
        
gridContainingAll :: [(Int,Int)] -> [(Int,Int)]
gridContainingAll points = [(x,y) | x <- [xMin..xMax], y <- [yMin..yMax]]
    where xMin = minimum xs
          xMax = maximum xs
          yMin = minimum ys
          yMax = maximum ys
          xs   = map fst points
          ys   = map snd points

distanceFrom :: (Int,Int) -> (Int,Int) -> Int
distanceFrom (x,y) (a,b) = (abs(x-a) + abs(y-b))

closest :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
closest points target = head $ groupBy ((==) `on` (distanceFrom target)) $ sortBy (comparing (distanceFrom target)) points

main :: IO ()
main = do
    input <- (readFile "../input")
    let points = map parsePoint $ lines input
    let voronoi = concat $ filter ((==1) . length) $ map (closest points) $ gridContainingAll points
    let infinitePoints = HS.fromList $ nub $ concat $ filter ((==1) . length) $ map (closest points) $ perimiterAround points
    let finitePoints = filter (\p -> p `HS.notMember` infinitePoints) voronoi
    print $ maximum $ map length $ group $ sort finitePoints