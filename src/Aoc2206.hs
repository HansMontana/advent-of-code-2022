module Aoc2206 (aoc2206) where

import qualified Data.List.Extra as List
import qualified Data.Set as Set

-- Part a and b

buildWindows :: Int -> String -> [String]
buildWindows _ [] = []
buildWindows n s = take n s : buildWindows n (drop 1 s)

toNumOfDistinctChar :: [String] -> [Int]
toNumOfDistinctChar = map (Set.size . Set.fromList)

-- Unsafe use of optional values in a Maybe, like using 'get' directly on a Java Optional.
-- Only the happy case 'Just (i::Int)' is considered here.
-- Pattern match(es) are non-exhaustive. Patterns of type ‘Maybe Int’ not matched: Nothing.
-- https://wiki.haskell.org/Maybe
firstOccurence :: Int -> [Int] -> Int
firstOccurence n ls = let Just pos = List.elemIndex n ls
                      in pos

solve :: Int -> String -> Int
solve n s = (+) n $ firstOccurence n $ toNumOfDistinctChar $ buildWindows n s

aoc2206 :: IO ()
aoc2206 = do 
    -- input <- readFile "./resources/input6test"
    input <- readFile "./resources/input6real"
    -- part a
    print $ solve 4 input
    -- part b
    print $ solve 14 input