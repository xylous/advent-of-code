import Data.List

main :: IO ()
main = do
    input <- fmap lines (readFile "input10.txt")
    let bad = map (corruptedParens [] []) input
    let corrupted = map snd bad
    let incomplete = map (map inverseParen . fst) bad
    let trueIncomplete = filterIncompleteParens incomplete corrupted
    let partTwoScores = sort (map incompleteParensValue trueIncomplete)
    print (sum (map corruptedParensValue (concat corrupted)))
    print (partTwoScores !! (length partTwoScores `div` 2))

filterIncompleteParens :: [String] -> [String] -> [String]
filterIncompleteParens (inc:incs) (corr:corrs)
    | null corr = inc : filterIncompleteParens incs corrs
    | otherwise = filterIncompleteParens incs corrs
filterIncompleteParens _ _ = []

corruptedParensValue :: Char -> Int
corruptedParensValue ')' = 3
corruptedParensValue ']' = 57
corruptedParensValue '}' = 1197
corruptedParensValue '>' = 25137
corruptedParensValue _ = 0

incompleteParensValue :: [Char] -> Int
incompleteParensValue parens = fst (mapAccumL (\acc p -> (acc * 5 + incompleteValue p, p)) 0 parens)

incompleteValue :: Char -> Int
incompleteValue ')' = 1
incompleteValue ']' = 2
incompleteValue '}' = 3
incompleteValue '>' = 4
incompleteValue _ = 0

isOpeningParens :: Char -> Bool
isOpeningParens ch
    | ch == '(' || ch == '[' || ch == '{' || ch == '<' = True
    | otherwise = False

isClosingParens :: Char -> Bool
isClosingParens ch = not (isOpeningParens ch)

inverseParen :: Char  -> Char
inverseParen ch
    | ch == '(' = ')'
    | ch == '[' = ']'
    | ch == '{' = '}'
    | ch == '<' = '>'
    | otherwise = ' '

isMatchingParens :: Char -> Char -> Bool
isMatchingParens ch1 ch2
    | ch1 == inverseParen ch2 = True
    | otherwise = False

pop :: [a] -> [a]
pop [] = []
pop xs = tail xs

push :: a -> [a] -> [a]
push elem [] = [elem]
push elem xs = elem : xs

corruptedParens :: [Char] -> [Char] -> [Char] -> ([Char],[Char])
corruptedParens stack badParens [] = (stack,badParens)
corruptedParens stack badParens parens
    | isOpeningParens p' = corruptedParens (push p' stack) badParens (pop parens)
    | otherwise =
        if isMatchingParens p' s' then
            corruptedParens (pop stack) badParens (pop parens)
        else
            corruptedParens (pop stack) (push p' badParens) (pop parens)
      where
        s' = head stack
        p' = head parens
