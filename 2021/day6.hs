data Fish = Fish
    { timer :: Int
    , count :: Int
    } deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- fmap (map read . words . map replace) (readFile "input6.txt") :: IO [Int]
    let fishes = parseFish input
    let partOneFishes = finalState 80 0 fishes
    let partTwoFishes = finalState 256 0 fishes
    print (countFish partOneFishes)
    print (countFish partTwoFishes)

replace :: Char -> Char
replace ',' = ' '
replace c = c

countFish :: [Fish] -> Int
countFish [] = 0
countFish ((Fish _ count):fs) = count + countFish fs

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x =  length . filter (==x)

parseFish :: [Int] -> [Fish]
parseFish ts = [Fish timer (countOccurrences timer ts) | timer <- [0..8]]

nextFishState :: Fish -> [Fish]
nextFishState (Fish 0 count) = [Fish 6 count, Fish 8 count]
nextFishState (Fish timer count) = [Fish (timer - 1) count]

concatFishes :: [Fish] -> [Fish]
concatFishes fishes = [Fish timer (fishOnTimer timer fishes) | timer <- [0..8]]

fishOnTimer :: Int -> [Fish] -> Int
fishOnTimer n fishes = countFish (filter (\x -> timer x ==n) fishes)

finalState :: Int -> Int -> [Fish] -> [Fish]
finalState maxDays currentDay s
    | maxDays == currentDay = s
    | otherwise = finalState maxDays (currentDay + 1) s'
      where
        s' = concatFishes (concatMap nextFishState s)
