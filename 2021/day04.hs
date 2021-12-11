import Data.List
import Data.Function

type Marked = Int

data Cell = Cell
    { val :: Int
    , m :: Marked
    } deriving (Show)

instance Eq Cell where
    (Cell _ x) == (Cell _ y) = x == y

instance Ord Cell where
    (Cell _ x) `compare` (Cell _ y) = x `compare` y

main :: IO ()
main = do
    input <- fmap lines (readFile "input04.txt")
    let usable = filter (/= "") input
    let moves = map read (words (map replace (head usable))) :: [Int]
    let values = concatMap words (tail usable)
    let boards = splitEvery 25 (map (\x -> Cell (read x :: Int) 0) values)
    let marked = map (markAll 0 moves) boards
    let ranks = winOrder marked
    print (score (head ranks))
    print (score (last ranks))

replace :: Char -> Char
replace ',' = ' '
replace c = c

splitEvery :: Ord a => Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

score :: (Cell,[Cell]) -> Int
score (c,cs) = val c * sum (map val (filter (\x -> m x > m c) cs))

markAll :: Int -> [Int] -> [Cell] -> [Cell]
markAll _ [] cs = cs
markAll n ms cs = markAll (n + 1) (tail ms) cs'
  where
    cs' = map (\x -> if val x == ms' then Cell (val x) n else x) cs
    ms' = head ms

winOrder :: [[Cell]] -> [(Cell,[Cell])]
winOrder cs = sort (zip wCell cs)
  where
    wCell = zipWith smallerTuple (map minRowPerBoard cs) (map minColPerBoard cs)
    smallerTuple c1 c2
        | y > x = c1
        | otherwise = c2
          where
            x = m c1
            y = m c2

minRowPerBoard :: [Cell] -> Cell
minRowPerBoard cs = minimum (maxRows cs)

maxRows :: [Cell] -> [Cell]
maxRows c = map maximum (rows c)

rows :: [Cell] -> [[Cell]]
rows = splitEvery 5

minColPerBoard :: [Cell] -> Cell
minColPerBoard cs = minimum (maxColumns cs)

maxColumns :: [Cell] -> [Cell]
maxColumns c = map maximum (columns c)

columns :: [Cell] -> [[Cell]]
columns = transpose . rows
