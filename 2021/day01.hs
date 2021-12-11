main :: IO ()
main = do
    input <- fmap lines (readFile "input01.txt")
    let ints = map read input :: [Int]
    let subdivided = chunk 3 ints
    print (solve ints)
    print (solve (map sum subdivided))

solve :: [Int] -> Int
solve [] = 0
solve [_] = 0
solve (d:d':ds)
    | d < d' = 1 + solve ds'
    | otherwise = 0 + solve ds'
      where
        ds' = d' : ds

-- Part Two
-- split a list into overlapping sub-lists of specified size
chunk :: Int -> [a] -> [[a]]
chunk n xs = chunk' n [] xs
    where
    chunk' _ results [] = results
    chunk' n results xs
        | length xs >= n = chunk' n (results ++ [take n xs]) $ tail xs
        | otherwise = chunk' n results []
