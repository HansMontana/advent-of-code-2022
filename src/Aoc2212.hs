module Aoc2212 (aoc2212) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe

-- Part a -- 💩 Naive implementation generating most possible partial paths at once. Works on example input but not on the real one. It's time for some backtracking instead.

type Coords = (Int , Int)
type Path = [Coords]
type Paths = [Path]
type Goal = Coords
-- origin: upper left
type Field = [[Char]]
data State = State Field Goal Paths deriving (Show)

type Dir = Coords

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

step :: State -> State
step (State fld goal paths) = let newPaths = map (stepPath fld) paths
                              in State fld goal (flatten newPaths)

stepPath :: Field -> Path -> Paths
stepPath fld (pos:poss) = let nextPoses = stepInValidDirs fld pos
                              nextPaths = map (: (pos:poss)) nextPoses
                              -- filtering out some partial paths is not enough to improve time performance and memory usage
                              firstOccurenceChecked = firstOccurenceInList nextPoses
                              onlyOnePathToCurrentPos = map snd $ filter fst $ zip firstOccurenceChecked nextPaths
                              noCyclePaths = filter hasNoCycle onlyOnePathToCurrentPos
                          in noCyclePaths

hasNoCycle :: Path -> Bool
hasNoCycle p = length p == unique p

stepInValidDirs :: Field -> Coords -> [Coords]
stepInValidDirs fld pos = let neighbors = map (add pos) [up, down, left, right]
                              inBounds = filter (isInBounds fld) neighbors
                              reachables = filter (reachable fld pos) inBounds
                          in reachables

reachable :: Field -> Coords -> Coords -> Bool
reachable fld cur next = let curVal = Char.ord $ valAt fld cur
                             nextVal = Char.ord $ valAt fld next
                         in (curVal + 1) >= nextVal

isInBounds :: Field -> Coords -> Bool
isInBounds fld (x,y) = let ((lowX, lowY), (highX, highY)) = bounds fld
                       in x>=lowX && x<highX && y>=lowY && y<highY 

runTillChecked :: (State -> Bool) -> State -> State
runTillChecked c s = if c s then s else runTillChecked c (step s)

check :: State -> Bool
check (State _ goal paths) = let heads = map head paths
                              in goal `List.elem` heads

run :: State -> State
run =  runTillChecked check

toSteps :: State -> Int
toSteps (State _ _ (p:_)) = length p - 1

solveA :: State -> Int
solveA input = toSteps $ run input

firstOccurenceInList :: Eq a => [a] -> [Bool] 
firstOccurenceInList = firstOccurenceInList' []
    where firstOccurenceInList':: Eq a => [a] -> [a] -> [Bool]
          firstOccurenceInList' _ [] = []
          firstOccurenceInList' visited  (l:ls) = if l `elem` visited then False: firstOccurenceInList' visited ls else True : firstOccurenceInList' (l:visited) ls

aoc2212 :: IO ()
aoc2212 = do 
    input <- readFile "./resources/input12test"
    -- This naive implementation does not work on the real input
    -- input <- readFile "./resources/input12real"
    -- part a
    let State fld goal ps = translate $ splitIntoLines input
        in print $ solveA $ State fld goal ps

translate :: [[Char]] -> State
translate = toState . readInput

toState :: (Field, Coords, Goal) -> State
toState (fld, start, dest) = State fld dest [[start]]

readInput:: [[Char]] -> (Field, Coords, Goal)
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