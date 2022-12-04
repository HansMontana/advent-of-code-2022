module Aoc2204 (aoc2204) where

import Lib
import qualified Data.List as List
import qualified Data.Bifunctor as Bifunctor

-- Warning: pattern matching in this exercise is not always exhaustive.
-- Part a

-- translate further

-- to find out: map for tuples 
mapToInt :: (String, String) -> (Int, Int)
mapToInt (x, y) = (readInt x, readInt y)

mapToIntPairList :: [((String, String),(String , String))] -> [((Int, Int),(Int , Int))]
mapToIntPairList = map $ Bifunctor.bimap mapToInt mapToInt

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

-- TODO read up on tuples and (un)curry
pairContainsFully :: ([Int], [Int]) -> Bool
pairContainsFully (x, y) = containsfully x y

solve :: [([Int],[Int])] -> Int
solve = sum . map ((\b -> if b then 1 else 0) . pairContainsFully)

-- part b

intersects :: [Int] -> [Int] -> Bool
intersects xs ys = not $ null $ xs `List.intersect` ys

-- to do read up on tuples and (un)curry
pairIntersects :: ([Int], [Int]) -> Bool
pairIntersects (x, y) = intersects x y

solve2 :: [([Int],[Int])] -> Int
solve2 = sum . map ((\ b -> if b then 1 else 0) . pairIntersects)

aoc2204 :: IO ()
aoc2204 = do 
    -- input <- readFile "./resources/input4test"
    input <- readFile "./resources/input4real"
    -- part a
    print $ solve $ translate $ toTuplesOfTuples input
    -- part b
    print $ solve2 $ translate $ toTuplesOfTuples input