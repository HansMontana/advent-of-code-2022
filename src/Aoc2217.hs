module Aoc2217 (aoc2217) where

import Lib
import qualified Data.List.Extra as List

-- Types, Data Structures and Pretty Printing

shaftWidth :: Int
shaftWidth = 7

-- origin top-left unless stated otherwise
type Coords = (Int, Int)
-- (distance from left wall, bottom relative to last rock)
spawnPoint :: Coords
spawnPoint = (2, 3)

type Dir = Coords
left :: Dir
left = (-1 , 0)
right :: Dir
right = (1, 0)
down :: Dir
down = (0, 1)

add :: Coords -> Coords -> Coords
add (x, y) (a, b) = (x + a, y + b)

currentPieceChr :: Char
currentPieceChr = '@'
errorChr :: Char
errorChr = '?'
rockChr :: Char
rockChr = '#'

type Rock = [[Char]]
wideI :: Rock
wideI = ["####"]
cross :: Rock
cross = [".#.", "###", ".#." ]
jay :: Rock
jay = [ "..#", "..#", "###" ]
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
                                                                   towerHeight = "height of tower of rocks:\n" ++ show (heightOfRestingRocks shaft)
                                                               in join "\n\n" [shaftStr, nextMove, nextPiece, dropped, towerHeight]

getNumOfRocksDropped :: State -> RocksDropped
getNumOfRocksDropped (State rd _ _ _ _) = rd

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
combineSymbols s _ '.' = s
combineSymbols _ _ _ = errorChr

--- Part a

toState :: String -> State
toState gasStreams = let rocksDropped = 0
                         (shaft, currentPiece) = toConfiguration [] (head endlessSupplyOfRocks)
                         movements = concat $ repeat $ addDownMoves gasStreams
                         restRocks = tail endlessSupplyOfRocks
                    in State rocksDropped shaft currentPiece movements restRocks

toConfiguration :: Shaft -> Rock -> Configuration
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
endlessSupplyOfRocks = concat $ repeat [wideI, cross, jay, tallI, oh]

addDownMoves :: [Char] -> [Char]
addDownMoves ls = List.intersperse 'v' ls ++ ['v']

step :: State -> State
step state@(State _ _ _ [] _) = state
step (State rocksDropped shaft currentPiece ('<':dirs) rocks) = State rocksDropped shaft (moveLeft currentPiece shaft) dirs rocks
step (State rocksDropped shaft currentPiece ('>':dirs) rocks) = State rocksDropped shaft (moveRight currentPiece shaft) dirs rocks
step (State rocksDropped shaft currentPiece ('v':dirs) rocks) = moveDown $ State rocksDropped shaft currentPiece dirs rocks

moveLeft :: CurrentPiece -> Shaft -> CurrentPiece
moveLeft current@(_, (0, _)) _ = current
moveLeft current@(rocks, pos) shaft
    | overlaps shaft (rocks, add pos left) = current
    | otherwise = (rocks, add pos left)

moveRight :: CurrentPiece -> Shaft -> CurrentPiece
moveRight current@(r:rocks, pos@(x, _)) shaft
    | overlaps shaft (r:rocks, add pos right) = current
    | length r + x < shaftWidth = (r:rocks, add pos right)
    | otherwise = current

moveDown :: State -> State
moveDown state@(State rocksDropped shaft (r, (x, y)) dirs rocks)
    | length r + y >= length shaft = restPieceAndPrepareNext state
    | overlaps shaft (r, add (x, y) down) = restPieceAndPrepareNext state
    | otherwise = State rocksDropped shaft (r, add (x, y) down) dirs rocks

restPieceAndPrepareNext :: State -> State
restPieceAndPrepareNext (State rocksDropped shaft current dirs rocks) = let rockRestedInShaft = toShaftWithCurrentPiece shaft current rockChr
                                                                            (nextShaft, nextPiece) = toConfiguration rockRestedInShaft $ head rocks
                                                                        in State (rocksDropped + 1) nextShaft nextPiece dirs (tail rocks)

-- Reuse pretty printing functionality, even though there is room for performance improvements here
overlaps :: Shaft -> CurrentPiece -> Bool
overlaps shaft current = List.isInfixOf [errorChr] $ concat $ toShaftWithCurrentPiece shaft current rockChr

heightOfRestingRocks :: Shaft -> Int
heightOfRestingRocks = length . dropWhileAir

simulate :: Int -> State -> State
simulate rocksDropped = whileDo (\st -> getNumOfRocksDropped st < rocksDropped) step

aoc2217 :: IO ()
aoc2217 = do
    -- input <- readFile "./resources/input17test"
    input <- readFile "./resources/input17real"
    -- part a
    print $ simulate 2022 $ toState input
    -- part b --- not feasible until further optimization, the number of rocks dropped alone is bigger than the Int space
    -- print $ simulate 1000000000000 $ toState input