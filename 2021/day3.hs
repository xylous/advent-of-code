import Data.List
import Data.Char (digitToInt)

main :: IO ()
main = do
    input <- fmap lines (readFile "input3.txt")
    let gamma = mostCommonBits input
    let epsilon = map flipBit gamma
    let oxygenRating = oxygenGenerator 0 input
    let co2Scrubber = co2Rating 0 input
    print (binToDec gamma * binToDec epsilon)
    print (binToDec oxygenRating * binToDec co2Scrubber)

binToDec :: String -> Int
binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

flipBit :: Char  -> Char
flipBit '1' = '0'
flipBit '0' = '1'
flipBit _ = undefined

mostCommonBits :: [String] -> [Char]
mostCommonBits bs = [mostCommon (map (!! y) bs) | y <- [0..l-1]]
  where
    l = length (head bs)

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

oxygenGenerator :: Int -> [String] -> String
oxygenGenerator _ [b] = b
oxygenGenerator n bs = oxygenGenerator (n + 1) bs'
  where
    bs' = filter (\x -> x !! n == mostCommon (map (!! n) bs)) bs

co2Rating :: Int -> [String] -> String
co2Rating _ [b] = b
co2Rating n bs = co2Rating (n + 1) bs'
  where
    bs' = filter (\x -> flipBit (x !! n) == mostCommon (map (!! n) bs)) bs
