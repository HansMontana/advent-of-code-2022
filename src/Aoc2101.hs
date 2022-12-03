module Aoc2101 (aoc2101) where

import Lib

-- This is the first execise of last year's Advent of Code as a warm up.
-- 21-01 part a

data Measurement = Increased | DecreasedEq
    deriving (Eq, Show)

pairUp :: [Int] -> [(Int, Int)]
pairUp [] = []
pairUp [_] = []
pairUp (x:y:ls) = (x,y):pairUp (y:ls)

cmpr :: (Int, Int) -> Measurement
cmpr (x,y) = if x<y then Increased else DecreasedEq

derive :: [(Int, Int)] -> [Measurement]
derive = map cmpr

countInc :: [Measurement] -> Int
countInc = foldr (\x acc -> if x == Increased then acc+1 else acc) 0

countIncreases :: [Int] -> Int
countIncreases ls = countInc $ derive $ pairUp ls

-- 21-01 part b
buildWindows :: [Int] -> [Int]
buildWindows [] = []
buildWindows [_] = []
buildWindows [_,_] = []
buildWindows (x:y:z:ls) = x+y+z:buildWindows (y:z:ls)

countIncreasesWindowed :: [Int] -> Int
countIncreasesWindowed ls = countIncreases $ buildWindows ls

aoc2101 :: IO ()
aoc2101 = do 
    -- input <- readFile "./resources/input1atest"
    input <- readFile "./resources/input1areal"
    -- part a
    print $ countIncreases $ splitIntoInts input
    -- part b
    print $ countIncreasesWindowed $ splitIntoInts input