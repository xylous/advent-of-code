import Data.List

data Movement = GoUp
              | GoDown
              | GoLeft
              | GoRight
              deriving (Show, Eq)

data Key = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | A | B | C | D
         deriving (Show, Enum, Eq, Ord)

main :: IO ()
main = do
    input <- fmap lines (readFile "input2.txt")
    let instructions = map parseInstructions input
    let resultPartOne = mapAccumL (\a b -> (computeDigit a b, computeDigit a b)) (toEnum 4) instructions
    let resultPartTwo = mapAccumL (\a b -> (revisedComputeDigit a b, revisedComputeDigit a b)) (toEnum 4) instructions
    print (concatMap (\x -> show (fromEnum x + 1)) (get2nd resultPartOne))
    print (concatMap keyToString (get2nd resultPartTwo))
      where
        get2nd (_, b) = b

parseMovement :: Char -> Movement
parseMovement 'L' = GoLeft
parseMovement 'R' = GoRight
parseMovement 'U' = GoUp
parseMovement 'D' = GoDown
parseMovement _ = undefined

parseInstructions :: String -> [Movement]
parseInstructions = map parseMovement

computeDigit :: Key -> [Movement] -> Key
computeDigit p [] = p
computeDigit p ms = computeDigit p' (tail ms)
  where
    p' = nextPosition p (head ms)

nextPosition :: Key -> Movement -> Key
nextPosition p m
    | m == GoLeft && fromEnum p `notElem` [0,3,6] = toEnum (fromEnum p - 1)
    | m == GoRight && fromEnum p `notElem` [2,5,8] = toEnum (fromEnum p + 1)
    | m == GoUp && fromEnum p `notElem` [0,1,2] = toEnum (fromEnum p - 3)
    | m == GoDown && fromEnum p `notElem` [6,7,8] = toEnum (fromEnum p + 3)
nextPosition p _ = p

revisedNextPosition :: Key -> Movement -> Key
revisedNextPosition p m
    | m == GoLeft && p' `notElem` [0,1,4,9,12] = toEnum (p' - 1)
    | m == GoRight && p' `notElem` [0,3,8,11,12] = toEnum (p' + 1)
    | m == GoUp && p' `notElem` [0,1,3,4,8] = toEnum (revisedUp p')
    | m == GoDown && p' `notElem` [4,8,9,11,12] = toEnum (revisedDown p')
      where
        p' = fromEnum p
revisedNextPosition p _ = p

revisedUp :: Int -> Int
revisedUp k
    | k' `elem` [2,12] = k' - 2
    | otherwise = k' - 4
      where
        k' = fromEnum k

revisedDown :: Int -> Int
revisedDown k
    | k' `elem` [0,10] = k' + 2
    | otherwise = k' + 4
      where
        k' = fromEnum k

revisedComputeDigit :: Key -> [Movement] -> Key
revisedComputeDigit p [] = p
revisedComputeDigit p ms = revisedComputeDigit p' (tail ms)
  where
    p' = revisedNextPosition p (head ms)

keyToString :: Key -> String
keyToString k
    | k' == 9 = "A"
    | k' == 10 = "B"
    | k' == 11 = "C"
    | k' == 12 = "D"
    | otherwise = show (k' + 1)
      where
        k' = fromEnum k
