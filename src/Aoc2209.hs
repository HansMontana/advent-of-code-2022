module Aoc2209 (aoc2209) where

import Lib
import qualified Data.Bifunctor as Bifunctor

-- types, data structures and methods to generate them

type Coords = (Int, Int)

data Dir = L | R | U | D | Cover | UL | UR | DL | DR deriving (Eq, Show)

fromString :: String -> Dir
fromString "L" = L 
fromString "R" = R
fromString "U" = U
fromString "D" = D

toDataStructure :: [(String, Int)] -> [(Dir, Int)] 
toDataStructure = map (Bifunctor.first fromString)

--- Part a

toSingleMoves :: [(Dir, Int)] -> [Dir]
toSingleMoves ls = flatten $ map (uncurry $ flip replicate) ls

moveStep :: Coords -> Dir -> Coords
moveStep (x, y) L = (x - 1 , y)
moveStep (x, y) R = (x + 1 , y)
moveStep (x, y) U = (x , y + 1)
moveStep (x, y) D = (x , y - 1)

move :: Coords -> [Dir] -> Coords
move = foldl moveStep

walk :: [Dir] -> Coords
walk = move (0, 0)

trace :: ([a] -> Coords) -> [a] -> [Coords]
trace f dirs = map f $ toPrefixes dirs

toPrefixes :: [a]  ->[[a]]
toPrefixes dirs = drop 1 $ mapWithIndex (flip take) $ replicate (length dirs) dirs ++ [dirs]

touch :: Coords -> Coords -> Bool
touch (x, y) (a, b) = abs (x - a) < 2 && abs (y - b) < 2

dirTo :: Coords -> Coords -> Dir
dirTo (x, y) (ox, oy) 
    | (x, y) == (ox, oy) = Cover
    | x < ox  = if y < oy then UR else (if y == oy then R else DR)
    | x == ox  = if y < oy then U else D
    | otherwise  = if y < oy then UL else (if y == oy then L else DL)

moveInDir :: Dir -> Coords -> Coords
moveInDir Cover (x, y) = (x, y)
moveInDir L (x, y) = (x-1, y)
moveInDir R (x, y) = (x+1, y)
moveInDir U (x, y) = (x, y+1)
moveInDir D (x, y) = (x, y-1)
moveInDir UL (x, y) = (x-1, y+1)
moveInDir UR (x, y) = (x+1, y+1)
moveInDir DL (x, y) = (x-1, y-1)
moveInDir DR (x, y) = (x+1, y-1)

followStep :: Coords -> Coords -> Coords
followStep (x, y) (ox, oy)
    | touch (x, y) (ox, oy) = (x, y)
    | otherwise = moveInDir (dirTo (x, y) (ox, oy)) (x, y)

follow :: Coords -> [Coords] -> Coords
follow = foldl followStep

hunt :: [Coords] -> Coords
hunt = follow (0, 0)

solveA ::  [(Dir, Int)]  -> Int
solveA = unique . trace hunt . trace walk . toSingleMoves

--- part b

solveB ::  [(Dir, Int)]  -> Int
solveB = unique . times 9 (trace hunt) . trace walk . toSingleMoves

aoc2209 :: IO ()
aoc2209 = do 
    -- input <- readFile "./resources/input9test"
    -- input <- readFile "./resources/input9test2"
    input <- readFile "./resources/input9real"
    -- part a: 6256, CPU time:   6.94s
    -- print $ solveA $ toDataStructure $ readInput input
    -- part b: 2665,CPU time:  40.71s
    print $ solveB $ toDataStructure $ readInput input  

readInput :: String -> [(String, Int)]
readInput input = map (Bifunctor.second readInt .  splitIntoPairs " ") $ splitIntoLines input