module Aoc2215 (aoc2215) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- Part a

type Coords = (Int, Int)
type Column = Int
type Row = Int
type SensorBeaconPair = (Coords, Coords)
type SensorEmptyEnv = (Coords, [Coords])
type State = Map.Map Row (Set.Set Column)

toCoords :: [String] -> Coords
toCoords (x:y:_) = (readInt x, readInt y)

-- 

toEmptyEnv :: SensorBeaconPair -> [Coords] -- SensorEmptyEnv -- TODO next remove ALL Beacons in the way, problay second parameter [SeasonBeaconPair] and unzip 
toEmptyEnv (s, b) = let dist = taxiDistance s b
                        env = toDiamond s dist
                    in env

toDiamond :: Coords -> Int -> [Coords]
toDiamond (x, y) dist = flatten $ map (generateLine (x, y) dist) [(-dist)..dist]

generateLine :: Coords -> Int -> Int -> [Coords] 
generateLine (x, row) dist rowOffset = map (\column -> (column, row + rowOffset)) [(x - dist + abs rowOffset)..(x + dist - abs rowOffset)]

taxiDistance :: Coords -> Coords -> Int
taxiDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

solveA input = toEmptyEnv $ input !! 2

aoc2215 :: IO ()
aoc2215 = do 
    inputTest <- readFile "./resources/input15test"
    inputReal <- readFile "./resources/input15real"
    -- part a
    -- x=13, y=2: x=15, y=3
    print $ solveA $ readInput inputTest

readInput :: String -> [SensorBeaconPair]
readInput s = map readLine $ splitIntoLines s
    where readLine :: String -> SensorBeaconPair
          readLine s = let (sen:bea:_) = map (toCoords . Split.splitOn ", y=") $ Split.splitOn ": closest beacon is at x=" $ drop 12 s
                       in (sen, bea)                        