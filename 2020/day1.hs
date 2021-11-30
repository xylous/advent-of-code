main :: IO()
main = do
    input <- fmap lines (readFile "input1.txt")
    let numbers = map read input :: [Int]
    print (solve1 numbers)
    print (solve2 numbers)

solve1 :: [Int] -> Int
solve1 x = head [a * b | a <- x, b <- x, a + b == 2020]

solve2 :: [Int] -> Int
solve2 x = head [a * b * c | a <- x, b <- x, c <- x, a + b + c == 2020]
