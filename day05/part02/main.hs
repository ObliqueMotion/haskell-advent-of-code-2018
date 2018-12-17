module Main where
import Data.Char

reactsWith :: Char -> Char -> Bool
reactsWith x y = (toUpper x == toUpper y) && x /= y

fullyReact :: String -> String
fullyReact = foldr react []
    where react x [] = [x]
          react x (y:ys)
            | x `reactsWith` y = ys
            | otherwise = (x:y:ys)

removeUnitFrom :: String -> Char -> String
removeUnitFrom polymer unit = filter (\u -> (toLower u) /= unit) polymer

main :: IO ()
main = do
    polymer <- (readFile "../input")
    mapM_ print $ zip ['a'..'z'] $ length . fullyReact . removeUnitFrom polymer <$> ['a'..'z']