module Aoc2204 (aoc2204) where

import Lib
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Char as Char

-- Part a

-- translate further

-- to find out: map for tuples 
mapToInt :: (String, String) -> (Int, Int)
mapToInt (x, y) = (readInt x, readInt y)

mapToIntPairList :: [((String, String),(String , String))] -> [((Int, Int),(Int , Int))]
mapToIntPairList = map (\(x, y) -> (mapToInt x, mapToInt y))

toRange :: (Int, Int) -> [Int]
toRange (x,y) = [x..y]

toRangePair :: ((Int,Int),(Int,Int)) -> ([Int],[Int])
toRangePair (x,y) = (toRange x,toRange y)

translate :: [((String, String),(String , String))] -> [([Int],[Int])]
translate ls = map toRangePair $ mapToIntPairList ls

-- solve

containsfully :: [Int] -> [Int] -> Bool
containsfully xs ys = let lenX = length xs
                          lenY = length ys
                          lenI = length $ xs `List.intersect` ys
                      in lenX == lenI || lenY == lenI

-- to do read up on tuples and (un)curry
pairContainsFully :: ([Int], [Int]) -> Bool
pairContainsFully (x, y) = containsfully x y

solve ls = sum $ map (\b -> if b then 1 else 0) $ map pairContainsFully ls

-- part b

intersects :: [Int] -> [Int] -> Bool
intersects xs ys = (length $ xs `List.intersect` ys) /= 0

-- to do read up on tuples and (un)curry
pairIntersects :: ([Int], [Int]) -> Bool
pairIntersects (x, y) = intersects x y

solve2 ls = sum $ map (\b -> if b then 1 else 0) $ map pairIntersects ls

aoc2204 :: IO ()
aoc2204 = do 
    --input <- readFile "./resources/input4test"
    input <- readFile "./resources/input4real"
    -- part a
    print $ solve $ translate $ readInput input
    -- part b
    print $ solve2 $ translate $ readInput input


readInput :: String -> [((String, String),(String , String))]
readInput input = map splitTupleIntoPairOfPairs $ map (splitIntoPairs ",") $ splitIntoLines input

splitIntoPairs :: String  -> String -> (String, String)
splitIntoPairs d s = let [x, y] = take 2 $ Split.splitOn d s
                   in (x,y)

splitTupleIntoPairOfPairs :: (String, String) -> ((String, String),(String, String))
splitTupleIntoPairOfPairs (x, y) = (splitIntoPairs "-" x, splitIntoPairs "-" y)

