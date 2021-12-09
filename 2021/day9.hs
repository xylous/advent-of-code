import Data.Char
import Data.List

data Point = Point
    { val :: Int
    , row :: Int
    , col :: Int
    } deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- fmap lines (readFile "input9.txt")
    let points = concat (snd (mapAccumL (\x y -> (x+1,parsePoints 0 x y)) 0 input))
    let nonZeroes = filter (\x -> val x /= 0) points  -- zeroes are always the lowest value
    let lowPoints = filter (\x -> isLowPoint x (neighbours x points)) nonZeroes
    print (sum (map riskValue lowPoints) + (length points - length nonZeroes))

riskValue :: Point -> Int
riskValue p = val p + 1

parsePoints :: Int -> Int -> String -> [Point]
parsePoints _ _  [] = []
parsePoints row col (s:ss) = Point (digitToInt s) row col : parsePoints (row + 1) col ss

neighbours :: Point -> [Point] -> [Point]
neighbours p points = [x | x <- points, horizontalNeighbour x || verticalNeighbour x]
  where
    horizontalNeighbour x = row x `elem` [row p - 1, row p + 1] && col x == col p
    verticalNeighbour x = col x `elem` [col p - 1, col p + 1] && row x == row p

isLowPoint :: Point -> [Point] -> Bool
isLowPoint point neighbours = null [x | x <- neighbours, val point >= val x]
