module Main where

import Data.List
import Data.List.Split

-- Exapmle ["123", "987"] -> (123,987)
toTuple :: [String] -> (Int,Int)
toTuple [x,y] = ((read x::Int),(read y::Int))

-- Example "123,987:" -> (123,987)
parseStartIndex :: [String] -> (Int,Int)
parseStartIndex (_:_:index:_) = toTuple $ splitOn "," $ index \\ ":"

-- Example "123x987" -> (123,987)
parseDimensions :: [String] -> (Int,Int)
parseDimensions (_:_:_:dimensions:_) = toTuple $ splitOn "x" dimensions

-- Example ["#1","@","49,222:","19x20"] -> [(49,222), (19,20)]
parseClaim :: [String] -> [(Int,Int)]
parseClaim claim = [parseStartIndex claim, parseDimensions claim]

-- Example: [(2, 5), (2,3)] -> [(3,6), (4,6), (3,7), (4,7), (3,8), (4,8)]
expandClaim :: [(Int,Int)] -> [(Int,Int)]
expandClaim [] = []
expandClaim [(xStart,yStart),(xOffset,yOffset)] = [(x,y) | x <- xs, y <- ys]
    where
    xs = map (+xStart) [1..xOffset]
    ys = map (+yStart) [1..yOffset]

(_,[(x1,y1),(xOffset1,yOffset1)]) `overlap` (_,[(x2,y2),(xOffset2,yOffset2)]) 
        | x1 > x2+xOffset2 = False
        | y1 > y2+yOffset2 = False
        | x1+xOffset1 < x2 = False
        | y1+yOffset1 < y2 = False
        | otherwise = True

uniqueClaims :: [(Int,[(Int,Int)])] -> [(Int,[(Int,Int)])]
uniqueClaims = reverse . nubBy (overlap) . reverse

main :: IO ()
main = do
    input <- (readFile "../input")
    let claims = zip [id | id <- [1..]] $ map parseClaim $ map words $ lines input
    print $ map fst $ filter (\claim -> all (not . overlap claim) (claims \\ [claim])) claims
