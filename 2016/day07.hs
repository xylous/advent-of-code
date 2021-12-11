data IPV7 = IPV7 String String deriving Show

main :: IO ()
main = do
    input <- fmap lines (readFile "input07.txt")
    let ips = map parseIPV7 input
    let haveTLS = filter supportsTLS ips
    let haveSSL = filter supportsSSL ips
    print (length haveTLS)
    print (length haveSSL)

chunk :: Int -> [a] -> [[a]]
chunk n xs =
  if   length chunk' < n then []
  else chunk' : chunk n (tail xs)
    where chunk' = take n xs

replace :: Char -> Char
replace '[' = ' '
replace ']' = ' '
replace c = c

-- We know that sections between square brackets always come between sections
-- without, so, neatly, their indexes are all even!
oddElementIndex :: Ord a => [a] -> [a]
oddElementIndex xs = [xs !! n | n <- [0..l], odd (n + 1)]
  where
    l = length xs - 1

evenElementIndex :: Ord a => [a] -> [a]
evenElementIndex xs = [xs !! n | n <- [0..l], even (n + 1)]
  where
    l = length xs - 1

parseIPV7 :: String -> IPV7
parseIPV7 s = IPV7 outside inside
  where
    outside = unwords (oddElementIndex s')
    inside = unwords (evenElementIndex s')
    s' = words (map replace s)

supportsTLS :: IPV7 -> Bool
supportsTLS (IPV7 outside inside)
    | hasABBA outside && not (hasABBA inside) = True
    | otherwise = False

hasABBA :: String -> Bool
hasABBA (a:b:c:d:xs)
    | a == d && b == c && a /= b = True
    | otherwise = hasABBA xs'
      where
        xs' = [b, c, d] ++ xs
hasABBA _ = False

isABA :: String -> Bool
isABA (a:b:c:xs)
    | a == c = True
    | otherwise = isABA xs'
      where
        xs' = [b,c] ++ xs
isABA _ = False

isABAPair :: String -> String -> Bool
isABAPair (a:b:c:_) (d:e:f:_)
    | a == c && a == e && b == d && d == f = True
    | otherwise = False
isABAPair _ _ = False

hasABAPairs :: [String] -> [String] -> Bool
hasABAPairs x y = or $ isABAPair <$> x <*> y

supportsSSL :: IPV7 -> Bool
supportsSSL (IPV7 out ins) = hasABAPairs gOut gIns
  where
    chOut = chunk 3 out
    chIns = chunk 3 ins
    gOut = filter isABA chOut
    gIns = filter isABA chIns
