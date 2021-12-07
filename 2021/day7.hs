import Data.List

main :: IO ()
main = do
    input <- fmap (map read . wordsWhen (==',')) (readFile "input7.txt") :: IO [Int]
    print (sum (map (\x -> abs (median input - x)) input))
    print (minimum [sum ([gaussSum (abs (x - y)) | y <- input]) | x <- [0..(maximum input)]])

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

median :: Ord a => [a] -> a
median list = sort list !! (length list `div` 2)

gaussSum :: Int -> Int
gaussSum n = n * (n + 1) `div` 2
