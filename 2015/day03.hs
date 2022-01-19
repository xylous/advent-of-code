import Data.List

data Pos = Pos
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- head . lines <$> readFile "input03.txt"
    print $ length . nub . snd $ mapAccumL (\x y -> (nextPos x y,x)) (Pos 0 0) input

nextPos :: Pos -> Char -> Pos
nextPos (Pos x y) c = case c of '>' -> Pos (x+1) y
                                '<' -> Pos (x-1) y
                                '^' -> Pos x (y+1)
                                'v' -> Pos x (y-1)
                                _ -> undefined
