module Aoc2218 (aoc2218) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Coord3 = (Int, Int, Int)

fromList :: [a] -> (a, a, a)
fromList (x:y:z:_) = (x, y, z)

-- Part a: assumpition: no duplicates, won't filter for them

-- being the same cube is not neighboring
neighboring :: Coord3 -> Coord3 -> Bool
neighboring (x, y, z) (a, b, c)
    | x == a && y == b = abs (z - c) == 1
    | x == a && z == c = abs (y - b) == 1
    | y == b && z == c = abs (x - a) == 1
    | otherwise = False

neighboringInList :: Coord3 -> [Coord3] -> Int
neighboringInList pos ls = length $ filter id $ map (neighboring pos) ls

pluckFromIndex :: Int -> [a] -> (a,[a])
pluckFromIndex 0 (l:ls) = (l, ls)
pluckFromIndex n (l:ls) = combine l $ pluckFromIndex (n -1) ls
    where combine :: a -> (a, [a]) -> (a, [a])
          combine l (x, ls) = (x, l:ls)

-- solveA :: [Coord3] -> Int
solveA coords = sum $ map ((-) 6 . uncurry neighboringInList) $ zipWith pluckFromIndex [0..] $ replicate (length coords) coords

aoc2218 :: IO ()
aoc2218 = do 
    input <- readFile "./resources/input18test"
    input <- readFile "./resources/input18real"
    -- part a
    print $ solveA $ translate $ readInput input
    -- putStrLn "\n"
    -- print $ neighboringInList (2,2,2) [(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]

translate :: [[String]] -> [Coord3]
translate = map (fromList . map readInt)

readInput :: String -> [[String]]
readInput = map (Split.splitOn ",") . splitIntoLines