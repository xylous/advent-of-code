import Data.List

data Entry = Entry
    { min :: Int
    , max :: Int
    , letter :: String
    , password :: String
    } deriving Show

main :: IO ()
main = do
    input <- fmap lines (readFile "input02.txt")
    let parsed = map (parseEntry . words) input
    print (solvePartOne parsed)
    print (solvePartTwo parsed)

replace :: Char -> Char
replace '-' = ' '
replace c = c

minmax :: String -> [String]
minmax s = words (map replace s)

parseEntry :: [String] -> Entry
parseEntry (a:b:c) = Entry (read min :: Int) (read max :: Int) b (concat c)
  where
    min = head mm
    max = mm !! 1
    mm = minmax a
parseEntry _ = Entry 0 0 "" ""

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (== c) str

checkCorrectPartOne :: Entry -> Bool
checkCorrectPartOne (Entry min max char pass)
    | min <= chars && chars <= max = True
    | otherwise = False
      where
        chars = countLetters pass (head char)

solvePartOne :: [Entry] -> Int
solvePartOne e = length (filter checkCorrectPartOne e)

checkCorrectPartTwo :: Entry -> Bool
checkCorrectPartTwo (Entry min max char pass)
    | (first /= last) && (first == c || last == c) = True
    | otherwise = False
      where
        first = pass !! (min - 1)
        last = pass !! (max - 1)
        c = head char

solvePartTwo :: [Entry] -> Int
solvePartTwo e = length (filter checkCorrectPartTwo e)
