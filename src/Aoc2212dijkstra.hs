module Aoc2212dijkstra (aoc2212dijkstra) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

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
                       in x>=lowX && x<highX && y>=lowY && y<highY

aoc2212dijkstra :: IO ()
aoc2212dijkstra = do 
    input <- readFile "./resources/input12test"
    -- This naive implementation does not work on the real input
    -- part a
    print $ readInput $ splitIntoLines input

-- translate :: [[Char]] -> State
-- translate = toState . readInput

-- toState :: (Field, Source, Target) -> State
-- toState (fld, start, dest) = State -- TODO

readInput:: [[Char]] -> (HeightField, Source, Target)
readInput chrs = let replaceLine = map (\c -> if c == 'S' then 'a' else (if c == 'E' then 'z' else c))
                     field = map replaceLine chrs
                     start = Maybe.fromJust $ fieldElemIndex 'S' chrs
                     dest = Maybe.fromJust $ fieldElemIndex 'E' chrs
                 in (field, start, dest)

fieldElemIndex :: Eq a => a -> [[a]] -> Maybe (Int, Int)
fieldElemIndex e fld = let maybeVal = List.elemIndex e $ flatten fld
                           ((_,_),(bound,_)) = bounds fld
                           toCoords x = (x `mod` bound, x `div` bound)
                       in fmap toCoords maybeVal