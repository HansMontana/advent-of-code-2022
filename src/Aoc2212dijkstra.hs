module Aoc2212dijkstra (aoc2212dijkstra) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.Char as Char

-- Part a -- 🚧 WIP (not done) Solve Day 12 less naively using Dijkstra's algorithm https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

type Coords = (Int , Int)
type Source = Coords
type Target = Coords
-- origin: upper left
type HeightField = [[Char]]
type DistanceField = [[Int]]
type PreviousNodeField = [[Coords]]
type UnvisitedQueue = [Coords]
data State = State HeightField DistanceField PreviousNodeField UnvisitedQueue Target deriving (Show)
type Result = (DistanceField, PreviousNodeField)
type Dir = Coords

undef :: Coords
undef = (minBound :: Int, minBound :: Int) -- any value not reachable on any field

infinity :: Int
infinity = maxBound :: Int

up :: Dir
up = (0, -1)
down :: Dir
down = (0, 1)
left :: Dir
left = (-1 , 0)
right :: Dir
right = (1, 0)

add :: Coords -> Coords -> Coords
add (x, y) (a, b) = (x + a, y + b)

valAt :: [[a]] -> Coords -> a
valAt ls (x,y) = (ls !! y) !! x

bounds :: [[a]] -> ((Int, Int),(Int, Int))
bounds (ls:lss) = ((0,0),(length ls, length (ls:lss)))

stepInValidDirs :: HeightField -> Coords -> [Coords]
stepInValidDirs fld pos = let neighbors = map (add pos) [up, down, left, right]
                              inBounds = filter (isInBounds fld) neighbors
                              reachables = filter (reachable fld pos) inBounds
                          in reachables

reachable :: HeightField -> Coords -> Coords -> Bool
reachable fld cur next = let curVal = Char.ord $ valAt fld cur
                             nextVal = Char.ord $ valAt fld next
                         in (curVal + 1) >= nextVal

isInBounds :: HeightField -> Coords -> Bool
isInBounds fld (x,y) = let ((lowX, lowY), (highX, highY)) = bounds fld
                       in x >= lowX && x < highX && y >= lowY && y < highY

initField :: [[a]] -> b -> [[b]]
initField lss e = map (map (const e)) lss

toCoordList :: [[a]] -> [Coords]
toCoordList lss = let (_, (x, y)) = bounds lss
                      xss = replicate y $ take x [0..]
                      ys = take y [0..]
                      coordField = map (\(xs, y) -> map (\x -> (x, y)) xs) $ zip xss ys
                  in  flatten coordField

-- TODO: set df at start coords to 0 here (,and do a pretty printer for the state, using ∞ for infinity and ⊥ for undefined)
initialize :: (HeightField, Source, Target) -> State
initialize (hf, s, t) = let df = initField hf infinity
                            pnf = initField hf undef
                            uq = toCoordList hf
                        in State hf df pnf uq t

aoc2212dijkstra :: IO ()
aoc2212dijkstra = do 
    input <- readFile "./resources/input12test"
    -- This naive implementation does not work on the real input
    -- part a
    print $ initialize $ translate $ splitIntoLines input

translate:: [[Char]] -> (HeightField, Source, Target)
translate chrs = let replaceLine = map (\c -> if c == 'S' then 'a' else (if c == 'E' then 'z' else c))
                     field = map replaceLine chrs
                     start = fieldElemIndex 'S' chrs
                     dest = fieldElemIndex 'E' chrs
                 in (field, start, dest)

fieldElemIndex :: Eq a => a -> [[a]] -> (Int, Int)
fieldElemIndex e fld = let linearIdx = indexOf e $ flatten fld
                           (_,(bound,_)) = bounds fld
                           toCoords x = (x `mod` bound, x `div` bound)
                       in toCoords linearIdx