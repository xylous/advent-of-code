import Data.List
import qualified Data.Set as S

data Heading = North
             | East
             | South
             | West
             deriving (Show, Ord, Eq)

data Turn = RightTurn
          | LeftTurn
          deriving Show

data Step = Step
    Turn
    Int
    deriving Show

data Location = Location
    { x :: Int
    , y :: Int
    , heading :: Heading
    } deriving (Show, Ord)

instance Eq Location where
    (Location x y _) == (Location x' y' _) = (x, y) == (x', y')

main :: IO ()
main = do
    input <- fmap (words . filter (/= ',')) (readFile "input01.txt")
    let steps = map strToStep input
    let l = Location 0 0 North
    let destination = moveAll l steps
    print (distance destination)
    print destination
    let visited = scanl (flip move) (Location 0 0 North) steps
        allVisited = allLocations visited
    let repeated = firstRepeatingLocation allVisited
    print repeated
    print $ distance <$> repeated

charToTurn :: Char -> Turn
charToTurn 'R' = RightTurn
charToTurn 'L' = LeftTurn
charToTurn _ = undefined

strToStep :: String -> Step
strToStep str = Step t n
  where
    t = charToTurn (head str)
    n = read (tail str) :: Int

travel :: Int -> Location -> Location
travel i (Location x y North) = Location x (y + i) North
travel i (Location x y South) = Location x (y - i) South
travel i (Location x y East) = Location (x + i) y East
travel i (Location x y West) = Location (x - i) y West

turn :: Heading -> Turn -> Heading
turn North LeftTurn = West
turn North RightTurn = East
turn East LeftTurn = North
turn East RightTurn = South
turn South LeftTurn = East
turn South RightTurn = West
turn West LeftTurn = South
turn West RightTurn = North

move :: Step -> Location -> Location
move (Step t n) (Location x y hdg) = travel n (Location x y hdg')
  where
    hdg' = turn hdg t

moveAll :: Location -> [Step] -> Location
moveAll l [] = l
moveAll l (s:ss) = foldl moveAll (move s l) [ss]

distance :: Location -> Int
distance (Location x y _) = abs x + abs y

-- part 2
-- ...mostly copied off the Internet because i'm not that smort.
allLocations :: [Location] -> [Location]
allLocations [] = []
allLocations [l] = [l]
allLocations (Location x y _:stop@(Location _ _ h):ps) =
    takeWhile (/= stop) (iterate (travel 1) (Location x y h))
    ++ allLocations (stop:ps)

firstRepeatingLocation :: [Location] -> Maybe Location
firstRepeatingLocation = f S.empty
    where
        coords (Location x y _) = (x, y)
        f _ [] = Nothing
        f s (p:ps)
          | S.member (coords p) s = Just p
          | otherwise             = f (S.insert (coords p) s) ps
