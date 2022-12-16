module Aoc2217 (aoc2217) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- Types, Data Structures and Pretty Printing

shaftWidth :: Int
shaftWidth = 7

-- origin top-left unless stated otherwise
type Coords = (Int, Int)
-- (distance from left wall, bottom relative to last rock)
spawnPoint :: Coords
spawnPoint = (2, 4)

type Dir = Coords
left :: Dir
left = (-1 , 0)
right :: Dir
right = (1, 0)
down :: Dir
down = (0, 1)

add :: Coords -> Coords -> Coords
add (x, y) (a, b) = (x + a, y + b)

-- leftChr :: Char
-- leftChr = '<'
-- rightChr :: Char 
-- rightChr = '>'
-- downChr :: Char
-- downChr = 'v'

currentPieceChr :: Char
currentPieceChr = '@'
errorChr :: Char
errorChr = '?'

type Rock = [[Char]]
wideI :: Rock
wideI = ["####"]
cross :: Rock
cross = [".#.", "###", ".#." ]
waluigi :: Rock
waluigi = [ "..#", "..#", "###" ]
tallI :: Rock
tallI = ["#","#","#","#"]
oh :: Rock
oh = ["##","##"]

type RocksDropped = Int
type Shaft = [[Char]]
type CurrentPiece = (Rock, Coords)
type Configuration = (Shaft, CurrentPiece)
data State = State RocksDropped Shaft CurrentPiece [Char] [Rock]

instance Show State where
    show (State rocksDropped shaft currentPiece moves nexts) = let shaftStr = join "\n" $ toShaftWithCurrentPiece shaft currentPiece currentPieceChr
                                                                   nextMove = "next move:\n" ++ [head moves]
                                                                   nextPiece = join "\n" ("next piece:" : head nexts)
                                                                   dropped = "dropped:\n" ++ show rocksDropped
                                                               in join "\n\n" [shaftStr, nextMove, nextPiece, dropped]

toShaftWithCurrentPiece :: Shaft -> CurrentPiece -> Char -> Shaft
toShaftWithCurrentPiece ls (r, (x, 0)) embedSymbol = let rockHeight = length r
                                                         linesToBeMerged = take rockHeight ls
                                                         mergedLines = zipWith (embedIntoLine embedSymbol x) r linesToBeMerged
                                                     in mergedLines ++ drop rockHeight ls
toShaftWithCurrentPiece (l:ls) (r, (x, y)) embedSymbol = l : toShaftWithCurrentPiece ls (r, (x, y - 1)) embedSymbol

embedIntoLine :: Char -> Int -> [Char] -> [Char] -> [Char]
embedIntoLine symbol 0 rs ls = let (subLs, restLs) = List.splitAt (length rs) ls
                                   mergedSymbols = zipWith (combineSymbols symbol) rs subLs
                                   in mergedSymbols ++ restLs
embedIntoLine symbol pos rs (l:ls) = l : embedIntoLine symbol (pos - 1) rs ls

combineSymbols :: Char -> Char -> Char -> Char
combineSymbols _ '.' c = c
combineSymbols s c '.' = s
combineSymbols _ _ _ = errorChr

--- Part a

toState :: String -> State
toState gasStreams = let rocksDropped = 0
                         (shaft, currentPiece) = toConfiguration [] (head endlessSupplyOfRocks)
                         movements = addDownMoves gasStreams
                         restRocks = tail endlessSupplyOfRocks
                    in State rocksDropped shaft currentPiece movements restRocks

toConfiguration :: Shaft -> Rock -> (Shaft, CurrentPiece)
toConfiguration shaft rock = let shaftWithoutAir = dropWhileAir shaft
                                 rockHeight = length rock
                                 airBufferHeight = snd spawnPoint + rockHeight
                                 newShaft = replicate airBufferHeight (replicate shaftWidth '.') ++ shaftWithoutAir
                             in (newShaft, (rock, (fst spawnPoint, 0)))

dropWhileAir :: Shaft -> Shaft
dropWhileAir [] = []
dropWhileAir whole@(l:ls)
    | any (/= '.') l = whole
    | otherwise = dropWhileAir ls

endlessSupplyOfRocks :: [Rock]
endlessSupplyOfRocks = concat $ repeat [wideI, cross, waluigi, tallI, oh]

addDownMoves :: [Char] -> [Char]
addDownMoves ls = List.intersperse 'v' ls ++ repeat 'v'

step :: State -> State
step state@(State rocksDropped shaft currentPiece [] rocks) = state
step (State rocksDropped shaft currentPiece ('<':dirs) rocks) = State rocksDropped shaft (moveLeft currentPiece) dirs rocks
step state@(State rocksDropped shaft currentPiece ('>':dirs) rocks) = State rocksDropped shaft (moveRight currentPiece) dirs rocks
step state@(State rocksDropped shaft currentPiece ('v':dirs) rocks) = state -- TODO moveDown (State rocksDropped shaft currentPiece dirs rocks)

moveLeft :: CurrentPiece -> CurrentPiece
moveLeft (rocks, (0, y)) = (rocks, (0, y))
moveLeft (rocks, pos) = (rocks, add pos left)

moveRight :: CurrentPiece -> CurrentPiece
moveRight current@(r:rocks, pos@(x, y)) 
    | length r + x < shaftWidth = (r:rocks, add pos right) 
    | otherwise = current

-- TODO
-- moveDown :: State -> State
-- moveDown (State rocksDropped shaft (r, (x, y)) dirs rocks)
--     | length r + y >= length shaft = toShaftWithCurrentPiece  
--     | otherwise = State rocksDropped shaft (r, (x, y) + down) dirs rocks

---

aoc2217 :: IO ()
aoc2217 = do
    input <- readFile "./resources/input17test"
    -- input <- readFile "./resources/input17real"
    -- part a
    print $ applyTimes step 2 $ toState input