data Triangle = Triangle
    { a :: Int
    , b :: Int
    , c :: Int
    } deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- readFile "input3.txt"
    let ints = map read (words input) :: [Int]
    let triangles1 = map triangle (group 3 ints)
    let triangles2 = map triangle (groupVertically ints)
    print (length (filter validTriangle triangles1))
    print (length (filter validTriangle triangles2))

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

groupVertically :: [Int] -> [[Int]]
groupVertically (a:a':a'':b:b':b'':c:c':c'':xs) = [a,b,c] : [a',b',c'] : [a'',b'',c''] : groupVertically xs
groupVertically _ = []

triangle :: [Int] -> Triangle
triangle s = Triangle a b c
  where
    a = head s
    b = s !! 1
    c = s !! 2

validTriangle :: Triangle -> Bool
validTriangle (Triangle a b c)
    | a + b > c && a + c > b && b + c > a = True
    | otherwise = False
