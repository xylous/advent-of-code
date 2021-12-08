data Display = Display
    { signals :: [String]
    , outputs :: [String]
    } deriving (Show, Eq, Ord)

main :: IO ()
main = do
    input <- fmap lines (readFile "input8.txt")
    let notes = map parseDisplay input
    print (sum (map uniqueDigitsInOutput notes))

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseDisplay :: String -> Display
parseDisplay s = Display signals outputs
  where
    signals = words (head split)
    outputs = words (last split)
    split = wordsWhen (=='|') s

uniqueDigitsInOutput :: Display -> Int
uniqueDigitsInOutput (Display _ outputs) = length (filter isUnique outputs)
  where
    isUnique :: String -> Bool
    isUnique x = length x `elem` [2,3,4,7]
