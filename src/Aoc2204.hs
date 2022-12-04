module Aoc2204 (aoc2204) where

import Lib
import qualified Data.List as List
import qualified Data.Bifunctor as Bifunctor

-- Part a

-- translate further

mapToInt :: (String, String) -> (Int, Int)
mapToInt = mapTuple readInt

mapToIntPairList :: [((String, String),(String , String))] -> [((Int, Int),(Int , Int))]
mapToIntPairList = map $ Bifunctor.bimap mapToInt mapToInt

toRange :: (Int, Int) -> [Int]
toRange (x,y) = [x..y]

toRangePair :: ((Int,Int),(Int,Int)) -> ([Int],[Int])
toRangePair = mapTuple toRange

translate :: [((String, String),(String , String))] -> [([Int],[Int])]
translate ls = map toRangePair $ mapToIntPairList ls

-- solve

containsfully :: [Int] -> [Int] -> Bool
containsfully xs ys = let lenX = length xs
                          lenY = length ys
                          lenI = length $ xs `List.intersect` ys
                      in lenX == lenI || lenY == lenI

pairContainsFully :: ([Int], [Int]) -> Bool
pairContainsFully = uncurry containsfully

solve :: [([Int],[Int])] -> Int
solve = sum . map ((\b -> if b then 1 else 0) . pairContainsFully)

-- part b

intersects :: [Int] -> [Int] -> Bool
intersects xs ys = not $ null $ xs `List.intersect` ys

pairIntersects :: ([Int], [Int]) -> Bool
pairIntersects = uncurry intersects

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