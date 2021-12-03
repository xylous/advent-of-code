import qualified Data.Map as M
import Data.List

data Room = Room
    { name :: String
    , id :: Int
    , freq :: [Char]
    } deriving Show

roomId (Room _ id _) = id
roomName (Room n _ _) = n

main :: IO ()
main = do
    input <- fmap lines (readFile "input4.txt")
    let rooms = map parseRoom input
    let valids = filter validRoom rooms
    let decrypted = map decryptRoomName valids
    print (sum (map roomId valids))
    print (roomId (head (filter (\x -> roomName x == "northpoleobjectstorage") decrypted)))

parseRoom :: String -> Room
parseRoom s = Room name id freq
  where
    name = concat (init sp')
    id = read (last sp') :: Int
    freq = last sp
    sp = words (map replace s)
    sp' = init sp
    replace '-' = ' '
    replace '[' = ' '
    replace ']' = ' '
    replace c = c

validRoom :: Room -> Bool
validRoom (Room name _ fr)
    | matchingChecksum name fr = True
    | otherwise = False

matchingChecksum :: String -> [Char] -> Bool
matchingChecksum s cs
    | all (== True) [isMatchingChar (fm !! n) (cs !! n) | n <- [0..4]] = True
    | otherwise = False
      where
        fm = frequencyMap s
        q = cs !! n
        n = 0

isMatchingChar :: (Char, Int) -> Char -> Bool
isMatchingChar (c,_) ch
    | c == ch = True
    | otherwise = False

frequencyMap :: String -> [(Char, Int)]
frequencyMap s = sortBy sortGT fq
  where
    fq = M.toList $ M.fromListWith (+) [(c, 1) | c <- s]
    sortGT (a1, b1) (a2, b2)
        | b1 < b2 = GT
        | b1 > b2 = LT
        | b1 == b2 = compare b1 b2
        | otherwise = undefined

decryptRoomName :: Room -> Room
decryptRoomName (Room n id f) = Room (map (shiftChar id) n) id f

alphabet = ['a'..'z'] :: String
alphaLoop = cycle alphabet :: String

shiftChar :: Int -> Char -> Char
shiftChar n c = dropWhile (/= c) alphaLoop !! max 0 n
