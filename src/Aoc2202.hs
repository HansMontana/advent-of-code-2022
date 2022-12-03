module Aoc2202 (aoc2202) where

import Lib

-- Warning: pattern matching in this exercise is not always exhaustive.
-- Part a

-- further translation of the input

data Move = Rock | Paper | Scissors deriving (Eq, Show)

-- no error handling
toMove :: String -> Move
toMove s
    | s == "A" || s == "X" = Rock
    | s == "B" || s == "Y" = Paper
    | s == "C" || s == "Z" = Scissors

translateTuple :: (String, String) -> (Move, Move)
translateTuple (a, b) = (toMove a, toMove b)

translate :: [(String, String)] -> [(Move, Move)]
translate = map translateTuple

-- solving the puzzle

evalMove :: Move -> Int
evalMove Rock = 1
evalMove Paper = 2
evalMove Scissors = 3

evalMyMove ::  (Move, Move)-> Int
evalMyMove (_,m) = evalMove m

evalComb :: (Move, Move) -> Int
evalComb (Rock, Paper) = 6
evalComb (Paper, Scissors) = 6
evalComb (Scissors, Rock) = 6
evalComb (Rock, Scissors) = 0
evalComb (Paper, Rock) = 0
evalComb (Scissors, Paper) = 0
evalComb (op, me) = 3

solve :: [(Move, Move)] -> Int
solve ls = sum (map evalMyMove ls) + sum (map evalComb ls)

-- part b

-- further translation of the input

data Strategy = Lose | Draw | Win deriving (Eq, Show)

-- no error handling
toStrategy :: String -> Strategy
toStrategy "X" = Lose
toStrategy "Y" = Draw
toStrategy "Z" = Win

translateTuple2 :: (String, String) -> (Move, Strategy)
translateTuple2 (a, b) = (toMove a, toStrategy b)

translate2 :: [(String, String)] -> [(Move, Strategy)]
translate2 = map translateTuple2

-- solving the puzzle

choose :: (Move, Strategy) -> (Move, Move)
choose (Rock, Lose) = (Rock, Scissors)
choose (Rock, Win) = (Rock, Paper)
choose (Paper, Lose) = (Paper, Rock)
choose (Paper, Win) = (Paper, Scissors)
choose (Scissors, Lose) = (Scissors, Paper)
choose (Scissors, Win) = (Scissors, Rock)
choose (op, Draw) = (op, op)

toMoves :: [(Move, Strategy)] -> [(Move, Move)]
toMoves = map choose

solve2 :: [(Move, Strategy)] -> Int
solve2 ls = solve $ toMoves ls

aoc2202 :: IO ()
aoc2202 = do 
    -- input <- readFile "./resources/input2test"
    input <- readFile "./resources/input2real"
    -- part a
    print $ solve $ translate $ toTuples input
    -- part b
    print $ solve2 $ translate2 $ toTuples input