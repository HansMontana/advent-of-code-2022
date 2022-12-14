module Aoc2205 (aoc2205) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.List.Split as Split

-- Part a and b

-- crate order function for part a
reverseCrates :: String -> String 
reverseCrates = List.reverse

-- crate order function for part b
letCratesBe:: String -> String 
letCratesBe = id

solve :: (String -> String) -> ([String], [[Int]]) -> String
solve orderFun game = let endState = play orderFun game 
                      in  map head endState

-- TODO Learn about state monads at some point in my life.
play :: (String -> String) -> ([String], [[Int]]) -> [String]
play _ (state, []) = state
play orderFun (state, moves) = let newState = executeAMove orderFun state (head moves)
                               in play orderFun (newState, tail moves)

executeAMove :: (String -> String) -> [String] -> [Int] -> [String]
executeAMove orderFun state move = let result = removeFromStack state move
                                   in addToStack orderFun result move

-- Warning: Pattern match(es) are non-exhaustive
removeFromStack :: [String] -> [Int] -> ([String], String)
removeFromStack board [n, src, _] = let [preStacks, stackList, postStacks] = chooseStack board src
                                        stack = head stackList
                                        crates = take n stack
                                        restStack = drop n stack
                                    in (preStacks ++ [restStack] ++ postStacks, crates)

-- Warning: Pattern match(es) are non-exhaustive
addToStack :: (String -> String) -> ([String], String) -> [Int] -> [String] 
addToStack orderFun (board, crates) [_, _ , dst] = let [preStacks, stackList, postStacks] = chooseStack board dst
                                                       stack = head stackList
                                                       orderedCrates = orderFun crates
                                                       moreStack = orderedCrates ++ stack
                                                   in preStacks ++ [moreStack] ++ postStacks

chooseStack :: [String] -> Int -> [[String]]
chooseStack ls n = let (xs, ys) = List.splitAt (n - 1) ls
                in [xs, take 1 ys, tail ys]

aoc2205 :: IO ()
aoc2205 = do 
    -- input <- readFile "./resources/input5test"
    input <- readFile "./resources/input5real"
    -- part a
    print $ solve reverseCrates $ prepare input
    -- part b
    print $ solve letCratesBe $ prepare input

-- Warning: Pattern match(es) are non-exhaustive
prepare :: String -> ([String],[[Int]])
prepare s = let [b,m] = Split.splitOn "\n\n" s
            in (prepareBoard b, prepareMoves m)

prepareBoard :: String -> [String]
prepareBoard b = map (dropLast . List.trim) $ takeFirstAndEveryFourth $ drop 1 $ List.transpose $ splitIntoLines b

-- This solution is quite naive, TODO maybe learn more about regular expressions in Haskell
-- a lot of cases missing in pattern matching.
takeFirstAndEveryFourth :: [String] -> [String]
takeFirstAndEveryFourth [] = [] 
takeFirstAndEveryFourth ls = head ls : takeFirstAndEveryFourth (drop 4 ls)

dropLast :: [a] -> [a]
dropLast ls = take (length ls - 1) ls

prepareMoves :: String -> [[Int]]
prepareMoves ms = map (map readInt . 
                           drop 1 . 
                           applyOnFirst (Split.splitOn "move ") . 
                           applyOnFirst (Split.splitOn " from ") . 
                           Split.splitOn " to ") 
                       $ splitIntoLines ms

applyOnFirst :: (a ->[a]) -> [a] -> [a]
applyOnFirst f ls = let [xs, ys] = f (head ls)
                    in xs : ys : tail ls