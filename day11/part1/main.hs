module Main where
import Data.Ord (comparing)
import Data.List

type Point = (Int,Int)
serialNumber = 2187

squareGrid :: Int -> Point -> [Point]
squareGrid dimension (xStart,yStart)= [(x,y) | x <- xs, y <- ys]
    where xs = [xStart..xStart+dimension-1]
          ys = [yStart..yStart+dimension-1]

powerLevelOf :: Point -> (Point,Int)
powerLevelOf (x,y) = ((x,y),powerLevel)
    where powerLevel = (x^2 * y + serialNumber * x) `quot` 100 `mod` 10 - 5

sumPowerSquare :: [(Point,Int)] -> (Point,Int)
sumPowerSquare powers = ((xMin,yMin),sumPower)
    where xMin = minimum $ map (fst.fst) powers
          yMin = minimum $ map (snd.fst) powers
          sumPower = sum $ map snd powers

maximumByDim :: Int -> (Point,Int)
maximumByDim d = maximumBy (comparing snd) $ map sumPowerSquare $ map (map powerLevelOf) $ map (squareGrid d) $ squareGrid (301-d) (11,1)

main = do
    print $ fst $ maximumByDim 3