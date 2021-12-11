import Data.Array
import Data.Char (digitToInt)

type Octopuses = Array (Int, Int) Int

main :: IO ()
main = do
    input <- fmap (concatMap (map digitToInt) . lines) (readFile "input11.txt")
    let octopuses = listArray ((0,0),(9,9)) input
    let result = simulate 100 0 octopuses
    print (fst result)

allIndexes :: Octopuses -> [(Int,Int)]
allIndexes octopuses = [(y,x) | y <- [lower..upper], x <- [lower..upper]]
  where
    lower = fst (fst octopusesBounds)
    upper = fst (snd octopusesBounds)
    octopusesBounds = bounds octopuses

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (y,x) =  [(y',x')
                    | y' <- [y,y - 1,y + 1]
                    , y' >= 0
                    , y' <= 9
                    , x' <- [x,x - 1,x + 1]
                    , x' >= 0
                    , x' <= 9
                    , (y',x') /= (y,x)
                    ]

numFlashes :: Octopuses -> Int
numFlashes octopuses = length [point | point <- allIndexes octopuses, octopuses ! point == 0]

simulate :: Int -> Int -> Octopuses -> (Int,Octopuses)
simulate 0 flashes octopuses = (flashes,octopuses)
simulate steps flashes octopuses = simulate (steps - 1) (flashes + flashes') octopuses'
  where
    flashes' = numFlashes octopuses'
    octopuses' = nextState (incrementOctopuses octopuses)

nextState :: Octopuses -> Octopuses
nextState octopuses
    | hasFlashes octopuses = nextState octopuses'
    | otherwise = octopuses
      where
        -- problem lies here; i'm too frustrated to attempt describing it
        octopuses' = resetEnergy (octopuses //
            [(point, octopuses ! point + numAdjacentFlashes point octopuses)
            | point <- allIndexes octopuses
            , octopuses ! point /= 0
            ])

hasFlashes :: Octopuses -> Bool
hasFlashes octopuses = not (null [x | x <- allIndexes octopuses, octopuses ! x > 9])

numAdjacentFlashes :: (Int,Int) -> Octopuses -> Int
numAdjacentFlashes pos octopuses = length [x | x <- neighbours pos, octopuses ! x > 9]

resetEnergy :: Octopuses -> Octopuses
resetEnergy octopuses = octopuses // [(point, 0)
                            | point <- allIndexes octopuses
                            , octopuses ! point > 9
                            ]

incrementOctopuses :: Octopuses -> Octopuses
incrementOctopuses octopuses = octopuses // [(point, octopuses ! point + 1)
                            | point <- allIndexes octopuses
                            ]
