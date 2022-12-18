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

solveA :: [Coord3] -> Int
solveA coords = sum $ map ((-) 6 . uncurry neighboringInList) $ zipWith pluckFromIndex [0..] $ replicate (length coords) coords

-- Part b -- Assumption: Exactly one droplet is modelled

-- fill :: [Coord3] -> [Coord3]
-- fill cs = map fill

fillLine :: Ord a => Enum a => a -> a -> [a]
fillLine x y
    | x == y = [x]
    | x < y = [x..y]
    | otherwise = [y..x]

fillPlane :: Ord a => Enum a => [(a, [a])] -> [(a, a)]
fillPlane [] = []
fillPlane ((k, poss):ls) = zip (repeat k) (fillLine (minimum poss) (maximum poss)) ++ fillPlane ls

fillCube :: Ord a => Enum a => [(a, [(a, a)])] -> [(a, a, a)]
fillCube [] = []
fillCube ((k, poss):ls) = let planeInput = splitByFst poss
                          in  map (\x -> (k, fst x, snd x)) (fillPlane planeInput) ++ fillCube ls

splitByFst :: Ord a => [(a , a)] -> [(a, [a])]
splitByFst ls = Map.toList $ splitByFst' Map.empty ls
    where splitByFst' :: Ord a => Map.Map a [a] -> [(a , a)] -> Map.Map a [a]
          splitByFst' m [] = m
          splitByFst' m ((k, v):ls) = splitByFst' (Map.insertWith (++) k [v] m) ls

fill :: Ord a => Enum a => [(a, a, a)] -> [(a, a, a)]
fill [] = []
fill ls = let cubeXYZ = fillCube $ splitByFst3 ls
              cubeXZY = map twist $ fillCube $ splitByFst3 (map twist ls)
              cubeYZX = map rotateRight $ fillCube $ splitByFst3 (map rotateLeft ls)
              cubeYXZ = map (rotateRight . twist) $ fillCube $ splitByFst3 (map (twist . rotateLeft) ls)
              cubeZXY = map rotateLeft $ fillCube $ splitByFst3 (map rotateRight ls)
              cubeZYX = map (rotateLeft . twist) $ fillCube $ splitByFst3 (map (twist . rotateRight) ls)
          in foldr1 List.intersect [cubeXYZ, cubeXZY, cubeYZX, cubeYXZ, cubeZXY, cubeZYX]

splitByFst3 :: Ord a => [(a, a, a)] -> [(a, [(a, a)])]
splitByFst3 ls = Map.toList $ splitByFst3' Map.empty ls
    where splitByFst3' :: Ord a => Map.Map a [(a, a)] -> [(a, a, a)] -> Map.Map a [(a, a)]
          splitByFst3' m [] = m
          splitByFst3' m ((k, x, y):ls) = splitByFst3' (Map.insertWith (++) k [(x,y)] m) ls

rotateLeft :: (a, b, c) -> (b, c, a)
rotateLeft (x, y, z) = (y, z, x)

rotateRight :: (a, b, c) -> (c, a, b)
rotateRight (x, y, z) = (z, x, y)

twist :: (a, b, c) -> (a, c, b)
twist (x, y, z) = (x, z, y)

solveB :: [Coord3] -> Int
solveB = solveA . fill

-----   

aoc2218 :: IO ()
aoc2218 = do 
    input <- readFile "./resources/input18mine" -- minimalcube
    input <- readFile "./resources/input18mine2" -- rect in x dir
    -- input <- readFile "./resources/input18test"
    -- input <- readFile "./resources/input18real"
    -- part a
    putStrLn "Part a:"
    print $ solveA $ translate $ readInput input
    -- part b: first try with 3 views 2582 is too low, second try with 6 views is still 2582
    putStrLn "\nPart b:"
    print $ solveB $ translate $ readInput input
    putStrLn "\nsome testing"
    print $ List.sort $ translate $ readInput input
    putStrLn "\n"
    print $ List.sort $ fill $ translate $ readInput input
    putStrLn "\n"
    -- real input: 1422 new elements
    print $ (List.sort $ fill $ translate $ readInput input) List.\\ (List.sort $ translate $ readInput input)

translate :: [[String]] -> [Coord3]
translate = map (fromList . map readInt)

readInput :: String -> [[String]]
readInput = map (Split.splitOn ",") . splitIntoLines