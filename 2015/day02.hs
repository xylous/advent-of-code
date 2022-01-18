import Data.List

main :: IO ()
main = do
    input <- fmap (filter (/= "")) $ lines <$> readFile "input02.txt"
    let parsed = map parseEntry input
    print $ sum $ map dimension parsed
    print $ sum $ map ribbon parsed

parseEntry :: String -> [Int]
parseEntry s = map read $ words $ map (\x -> if x == 'x' then ' ' else x) s :: [Int]

dimension :: [Int] -> Int
dimension (a:b:c:_) = 2 * (a' + b' + c') + minimum [a',b',c']
  where
    a' = a * b
    b' = b * c
    c' = a * c
dimension _ = undefined

ribbon :: [Int] -> Int
ribbon (a:b:c:_) = a * b * c + minimum [a',b',c']
  where
    a' = 2 * (a + b)
    b' = 2 * (b + c)
    c' = 2 * (a + c)
ribbon _ = undefined
