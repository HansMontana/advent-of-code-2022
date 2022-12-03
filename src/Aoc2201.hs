module Aoc2201 (aoc2201) where

import Lib
import qualified Data.List as List

-- 22-01 part a

sumUp :: [[Int]] -> [Int]
sumUp = map sum

pickMostCalories :: [[Int]] -> Int
pickMostCalories lss = foldr max 0 $ sumUp lss

-- 22-01 part b

pick3MostCalories :: [[Int]] -> Int
pick3MostCalories lss =
    let ls = sumUp lss
        most = maximum ls
        fewerLs = List.delete most ls
        more = maximum fewerLs
        evenFewerLs = List.delete more fewerLs
        some = maximum evenFewerLs
    in most + more + some

-- Some alternative solutions to part b

-- recursive version of the naive version above (no error handling)
pick3MostCaloriesRecursive :: [[Int]] -> Int
pick3MostCaloriesRecursive lss = pickNMostCalories (sumUp lss) 3

pickNMostCalories :: [Int] -> Int -> Int
pickNMostCalories ls times
         | times <= 0 = 0
         | otherwise = let most = maximum ls
                           fewerLs = List.delete most ls
                       in most + pickNMostCalories fewerLs (times - 1)

-- based on sorting, mostly using predefined library functions
pick3MostCaloriesAfterSorting :: [[Int]] -> Int
pick3MostCaloriesAfterSorting = sum . take 3 . List.reverse . List.sort . sumUp 

aoc2201 :: IO ()
aoc2201 = do 
    -- input <- readFile "./resources/input1test"
    input <- readFile "./resources/input1real"
    -- part a
    print $ pickMostCalories $ splitIntoListsOfInt input
    -- part b
    print $ pick3MostCalories $ splitIntoListsOfInt input
    print $ pick3MostCaloriesRecursive $ splitIntoListsOfInt input
    print $ pick3MostCaloriesAfterSorting $ splitIntoListsOfInt input