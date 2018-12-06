module Main where
import Data.Char

data Result = Reducible | MaybeReducible | Irreducible deriving(Eq)

react :: (String,Result) -> (String,Result)
react ((x:y:ys),result)
    | x `reactsWith` y = react (ys,Reducible)
    | otherwise = (x:(fst reacted),snd reacted)
    where reacted = react ((y:ys),result)
          reactsWith x y = (toUpper x) == (toUpper y) && x /= y
react (end,result)
    | result == Reducible      = (end,MaybeReducible)
    | result == MaybeReducible = (end,Irreducible)

fullyReact :: String -> String
fullyReact polymer = fst $ head $ dropWhile (reducible) $ iterate react (polymer,MaybeReducible)
    where reducible = (==MaybeReducible) . snd

removeUnitFrom :: String -> Char -> String
removeUnitFrom polymer unit = filter (\u -> (toLower u) /= unit) polymer

main :: IO ()
main = do
    polymer <- (readFile "../input")
    mapM_ print $ zip ['a'..'z'] $ map (length . fullyReact . (removeUnitFrom polymer)) ['a'..'z'] 