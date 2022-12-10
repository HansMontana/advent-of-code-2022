module Aoc2210 (aoc2210) where

import Lib
import qualified Data.List.Extra as List

data Instruction = Noop | AddX Int  deriving (Eq, Show)
type SystemState = (Int, Int) -- (regX, next summand)

fromString :: String -> Instruction
fromString "noop" = Noop
fromString s = AddX $ readInt $ drop 5 s

-- Part a

cycles :: [Int]
cycles = [20, 60, 100, 140, 180, 220]

cyclesZeroIndexed :: [Int]
cyclesZeroIndexed = map (-1 +) cycles

startX :: Int
startX = 1

horizontalResolution :: Int
horizontalResolution = 40

apply :: [SystemState] -> Instruction -> [SystemState]
apply states Noop = [last states]
apply states (AddX y) = let (regX, addend) = last states
                        in [(regX + addend, y), (regX + y, 0)]

run :: [Instruction] -> [SystemState]                            
run input = flatten $ foldlList apply [(startX, 0)] input

toRegXValues :: [Instruction] -> [Int]
toRegXValues input = map fst $ run input

solveA :: [Instruction] -> Int
solveA input = sum $ zipWith (*) cycles $ map (toRegXValues input !!) cyclesZeroIndexed

-- Part B

sync :: [Int] -> [Bool]
sync xs = zipWith (\x scanPos -> abs (x - scanPos) <=1) xs $ flatten $ repeat [0..39]

toPixels :: [Bool] -> String
toPixels = map (\b -> if b then '#' else '.')

draw :: String -> [String]
draw str = reverse $ draw' [] str
    where draw' :: [String] -> String -> [String]
          draw' ls [] = ls
          draw' ls s = let (x, y) = List.splitAt horizontalResolution s
                       in draw' (x:ls) y

solveB :: [Instruction] -> [String]
solveB input = draw $ toPixels $ sync $ toRegXValues input

aoc2210 :: IO ()
aoc2210 = do 
    -- input <- readFile "./resources/input10test"
    -- input <- readFile "./resources/input10test2"
    input <- readFile "./resources/input10real"
    -- part a: 13440
    print $ solveA $ translateInput input
    -- part b: 
    -- "###..###..####..##..###...##..####..##.."
    -- "#..#.#..#....#.#..#.#..#.#..#....#.#..#."
    -- "#..#.###....#..#....#..#.#..#...#..#..#."
    -- "###..#..#..#...#.##.###..####..#...####."
    -- "#....#..#.#....#..#.#.#..#..#.#....#..#."
    -- "#....###..####..###.#..#.#..#.####.#..#."
    printList $ solveB $ translateInput input

translateInput :: String -> [Instruction]
translateInput = map fromString . splitIntoLines

-- Some pretty printing functions

{- prettyStates :: [CpuState] -> [String]
prettyStates = zipWith prettyState [0..]

prettyState :: Int -> CpuState -> String
prettyState i s = show i++":   "++show s -}