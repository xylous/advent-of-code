import Data.List
import Data.Char (digitToInt)

main :: IO ()
main = do
    input <- fmap lines (readFile "input3.txt")
    let gamma = mostCommonBits input
    let epsilon = map flipBit gamma
    let oxygenRating = partTwo 0 input True
    let co2Rating = partTwo 0 input False
    print (binToDec gamma * binToDec epsilon)
    print (binToDec oxygenRating * binToDec co2Rating)

binToDec :: String -> Int
binToDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

flipBit :: Char  -> Char
flipBit '1' = '0'
flipBit '0' = '1'
flipBit _ = undefined

mostCommonBits :: [String] -> [Char]
mostCommonBits bs = [mostCommon (map (!! y) bs) | y <- [0..l]]
  where
    l = length (head bs) - 1

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

partTwo :: Int -> [String] -> Bool -> String
partTwo _ [b] _ = b
partTwo n bs shouldMatchMost
    | shouldMatchMost = partTwo (n + 1) most shouldMatchMost
    | otherwise = partTwo (n + 1) least shouldMatchMost
      where
        most = filter (\x -> x !! n == mostCommon (map (!! n) bs)) bs
        least = filter (\x -> flipBit (x !! n) == mostCommon (map (!! n) bs)) bs
