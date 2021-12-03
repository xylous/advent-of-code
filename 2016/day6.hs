import Data.List

main :: IO ()
main = do
    input <- fmap lines (readFile "input6.txt")
    let l = length (head input) - 1
    let partOneCharacters = [mostCommon (map (!! n) input) | n <- [0..l]]
    let partTwoCharacters = [leastCommon (map (!! n) input) | n <- [0..l]]
    print partOneCharacters
    print partTwoCharacters

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = snd . minimum . map (\xs -> (length xs, head xs)) . group . sort
