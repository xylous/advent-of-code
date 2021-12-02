data Direction = Forward | Down | Up
                deriving (Show, Eq)

data Command = Command
    { dir :: Direction
    , n :: Int
    } deriving (Show, Eq)

data Position = Position Int Int Int
                deriving (Show, Eq)

main :: IO ()
main = do
    input <- fmap lines (readFile "input2.txt")
    let cmds = map (parseCommand . words) input
    let partOne = moveAll (Position 0 0 0) cmds
    let partTwo = revisedMoveAll (Position 0 0 0) cmds
    print (positionProduct partOne)
    print (positionProduct partTwo)

positionProduct :: Position -> Int
positionProduct (Position x y a) = x * y

parseCommand :: [String] -> Command
parseCommand (d:n:_) = Command dir n'
  where
    dir = chooseDirection d
      where
        chooseDirection "forward" = Forward
        chooseDirection "down" = Down
        chooseDirection "up" = Up
        chooseDirection _ = undefined
    n' = read n :: Int
parseCommand _ = undefined

moveAll :: Position -> [Command] -> Position
moveAll p [] = p
moveAll p (c:cs) = moveAll p' cs
  where
    p' = moveNext p c

moveNext :: Position -> Command -> Position
moveNext (Position x y a) (Command Forward n) = Position (x + n) y a
moveNext (Position x y a) (Command Down n) = Position x (y + n) a
moveNext (Position x y a) (Command Up n) = Position x (y - n) a

revisedMoveAll :: Position -> [Command] -> Position
revisedMoveAll p [] = p
revisedMoveAll p (c:cs) = revisedMoveAll p' cs
  where
    p' = revisedMoveNext p c

revisedMoveNext :: Position -> Command -> Position
revisedMoveNext (Position x y a) (Command Forward n) = Position (x + n) y' a
  where
    y' = y + a * n
revisedMoveNext (Position x y a) (Command Down n) = Position x y (a + n)
revisedMoveNext (Position x y a) (Command Up n) = Position x y (a - n)
