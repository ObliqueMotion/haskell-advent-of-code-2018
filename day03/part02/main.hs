module Main where
import Data.List
import Data.List.Split

type Point = (Int,Int)

-- Exapmle ["123", "987"] -> (123,987)
toTuple :: [String] -> Point
toTuple [x,y] = ((read x::Int),(read y::Int))

-- Example ["#7","@","49,222:","19x20"] -> (49,222)
parseIndex :: [String] -> Point
parseIndex (_:_:index:_) = toTuple $ splitOn "," $ index \\ ":"

-- Example ["#7","@","49,222:","19x20"] -> (19,20)
parseDimensions :: [String] -> (Int,Int)
parseDimensions (_:_:_:dimensions:_) = toTuple $ splitOn "x" dimensions

-- Example ["#7","@","49,222:","19x20"] -> 7
parseId :: [String] -> Int
parseId (id:_) = read (id \\ "#")

-- Example ["#7","@","49,222:","19x20"] -> (7, [(49,222), (19,20)])
parseClaim :: [String] -> (Int,[Point])
parseClaim claim = (id, [index, dimensions])
    where
    id = parseId claim
    index = parseIndex claim
    dimensions = parseDimensions claim

(_,[(x1,y1),(xOffset1,yOffset1)]) `overlaps` (_,[(x2,y2),(xOffset2,yOffset2)]) 
        | x1 > x2+xOffset2 = False
        | y1 > y2+yOffset2 = False
        | x1+xOffset1 < x2 = False
        | y1+yOffset1 < y2 = False
        | otherwise = True

main :: IO ()
main = do
    input <- (readFile "../input")
    let claims = map (parseClaim . words) $ lines input
    print $ map fst $ filter (\claim -> all (not . overlaps claim) (claims \\ [claim])) claims
