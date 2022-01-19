import Data.List

data Pos = Pos
    { x :: Int
    , y :: Int
    } deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- head . lines <$> readFile "input03.txt"
    -- first part
    print $ length . nub . snd $ mapAccumL (\x y -> (nextPos x y,x)) (Pos 0 0) input
    -- second part: we only need to change the input!
    let santa = snd $ mapAccumL (\x y -> (nextPos x y,nextPos x y)) (Pos 0 0) $ evens input
    let robosanta = snd $ mapAccumL (\x y -> (nextPos x y,nextPos x y)) (Pos 0 0) $ odds input
    print $ length . nub $ santa ++ robosanta ++ [Pos 0 0]

odds :: [a] -> [a]
odds xs = [xs !! n | n <- [1,3..length xs-1]]

evens :: [a] -> [a]
evens xs = [xs !! n | n <- [0,2..length xs-1]]

nextPos :: Pos -> Char -> Pos
nextPos (Pos x y) c = case c of '>' -> Pos (x+1) y
                                '<' -> Pos (x-1) y
                                '^' -> Pos x (y+1)
                                'v' -> Pos x (y-1)
                                _ -> undefined
