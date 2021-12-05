import Data.List

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Ord)

data Cloud = Cloud Point Point
            deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- fmap lines (readFile "input5.txt")
    let clouds = map parseCloud input
    let points = map allPointsInHorizontalCloud clouds
    print (length (getDups (concat points)))

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

allPointsInHorizontalCloud :: Cloud -> [Point]
allPointsInHorizontalCloud (Cloud (Point x1 y1) (Point x2 y2))
    | x1 == x2 && y1 > y2 = a : allPointsInHorizontalCloud y1Smaller
    | x1 == x2 && y2 > y1 = b : allPointsInHorizontalCloud y2Smaller
    | y1 == y2 && x1 > x2 = a : allPointsInHorizontalCloud x1Smaller
    | y1 == y2 && x2 > x1 = b : allPointsInHorizontalCloud x2Smaller
    | x1 == x2 && y1 == y2 = [a]
    | otherwise = []
      where
        a = Point x1 y1
        b = Point x2 y2
        y1Smaller = Cloud (Point x1 (y1 - 1)) (Point x2 y2)
        y2Smaller = Cloud (Point x1 y1) (Point x2 (y2 - 1))
        x1Smaller = Cloud (Point (x1 - 1) y1) (Point x2 y2)
        x2Smaller = Cloud (Point x1 y1) (Point (x2 - 1) y2)
