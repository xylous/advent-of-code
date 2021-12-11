import Data.List

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Ord)

data Cloud = Cloud Point Point
            deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- fmap lines (readFile "input05.txt")
    let clouds = map parseCloud input
    let horizontalLines = map (allPointsInCloud False) clouds
    let withDiagonals = map (allPointsInCloud True) clouds
    print (length (getDups (concat horizontalLines)))
    print (length (getDups (concat withDiagonals)))

getDups :: Ord a => [a] -> [a]
getDups = map head . filter (\l -> length l > 1) . group . sort

replace :: Char -> Char
replace ',' = ' '
replace c = c

parsePoint :: String -> Point
parsePoint s = Point a b
  where
    a = head nums
    b = last nums
    nums = map read (words (map replace s)) :: [Int]

parseCloud :: String -> Cloud
parseCloud s = Cloud (parsePoint a) (parsePoint b)
  where
    a = head points
    b = last points
    points = words s

allPointsInCloud :: Bool -> Cloud -> [Point]
allPointsInCloud matchDiagonals (Cloud (Point x1 y1) (Point x2 y2))
    | x1 == x2 && y1 > y2 = a : allPointsInCloud matchDiagonals straightY1
    | x1 == x2 && y2 > y1 = b : allPointsInCloud matchDiagonals straightY2
    | y1 == y2 && x1 > x2 = a : allPointsInCloud matchDiagonals straightX1
    | y1 == y2 && x2 > x1 = b : allPointsInCloud matchDiagonals straightX2
    | matchDiagonals && x1 > x2 && y1 > y2 = b : allPointsInCloud matchDiagonals diagX1Y1
    | matchDiagonals && x2 > x1 && y2 > y1 = a : allPointsInCloud matchDiagonals diagX2Y2
    | matchDiagonals && x2 > x1 && y1 > y2 = a : allPointsInCloud matchDiagonals diagX2Y1
    | matchDiagonals && x1 > x2 && y2 > y1 = a : allPointsInCloud matchDiagonals diagX1Y2
    | x1 == x2 && y1 == y2 = [a]
    | otherwise = []
      where
        a = Point x1 y1
        b = Point x2 y2
        straightY1 = Cloud (Point x1 (y1 - 1)) (Point x2 y2)
        straightY2 = Cloud (Point x1 y1) (Point x2 (y2 - 1))
        straightX1 = Cloud (Point (x1 - 1) y1) (Point x2 y2)
        straightX2 = Cloud (Point x1 y1) (Point (x2 - 1) y2)
        diagX1Y1 = Cloud (Point x1 y1) (Point (x2 + 1) (y2 + 1))
        diagX2Y2 = Cloud (Point (x1 + 1) (y1 + 1)) (Point x2 y2)
        diagX2Y1 = Cloud (Point (x1 + 1) (y1 - 1)) (Point x2 y2)
        diagX1Y2 = Cloud (Point (x1 - 1) (y1 + 1)) (Point x2 y2)
