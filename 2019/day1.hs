main :: IO ()
main = do
    input <- fmap lines (readFile "input1.txt")
    let masses = map read input :: [Int]
    let partOne = sum (map fuel masses)
    let partTwo = sum (map revisedFuel masses)
    print partOne
    print partTwo

fuel :: Int -> Int
fuel m = m `div` 3 - 2

-- Why greater than 5? Because 5 `div` - 2 = 0, and the puzzle tells us to
-- treat values lower than 0 as 0
revisedFuel :: Int -> Int
revisedFuel m
    | m > 5 = f + revisedFuel f
    | otherwise = 0
      where
        f = m `div` 3 - 2
