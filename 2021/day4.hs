import Data.List
import Data.Function

type Marked = Int

data Cell = Cell
    { val :: Int
    , m :: Marked
    } deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- fmap lines (readFile "input4.txt")
    let usable = filter (/= "") input
    let moves = map read (words (map replace (head usable))) :: [Int]
    let values = concatMap words (tail usable)
    let boards = splitEvery 25 (map (\x -> Cell (read x :: Int) 0) values)
    let marked = map (markAll 0 moves) boards
    let win = winner marked
    print (score win)

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

winner :: [[Cell]] -> (Cell,[Cell])
winner cs = (wCell,wBoard)
  where
    wCell = minimumBy compareCell (map minRowPerBoard cs ++ map minColPerBoard cs)
    wBoard = head (filter isOriginalBoard cs)
      where
        isOriginalBoard :: [Cell] -> Bool
        isOriginalBoard b
            | wCell == minRowPerBoard b || wCell == minColPerBoard b = True
            | otherwise = False

minRowPerBoard :: [Cell] -> Cell
minRowPerBoard cs = minimumBy compareCell (maxRows cs)

maxRows :: [Cell] -> [Cell]
maxRows c = map (maximumBy compareCell) (rows c)

rows :: [Cell] -> [[Cell]]
rows = splitEvery 5

minColPerBoard :: [Cell] -> Cell
minColPerBoard cs = minimumBy compareCell (maxColumns cs)

maxColumns :: [Cell] -> [Cell]
maxColumns c = map (maximumBy compareCell) (columns c)

columns :: [Cell] -> [[Cell]]
columns = transpose . rows

compareCell = compare `on` m
