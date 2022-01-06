import Data.List (mapAccumL, elemIndex)

main :: IO ()
main = do
    input <- readFile "input01.txt"
    print $ fst $ mapAccumL (\x y -> (if y == '(' then x + 1 else x - 1,y)) 0 input
    print $ elemIndex '1' . snd $ mapAccumL (\x y -> (if y == '(' then x + 1 else x - 1,if x == -1 then '1' else '0')) 0 input
