module Aoc2214 (aoc2214) where

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
type Rocks = [Coords]
type Sand = [Coords]
type Source = Coords
type Grain = Coords
data Cave = Cave Rocks Source Sand Grain deriving (Eq, Show) -- TODO: Pretty printer will provide instance of Show
type Dir = Coords
-- up :: Dir
-- up = (0, -1)
down :: Dir
down = (0, 1)
-- left :: Dir
-- left = (-1 , 0)
-- right :: Dir
-- right = (1, 0)
downLeft :: Dir
downLeft = (-1 , 1)
downRight :: Dir
downRight = (1, 1)

undef :: Coords
undef = (minBound :: Int, minBound :: Int) -- any value not reachable in the cave

bounds :: [[a]] -> ((Int, Int),(Int, Int))
bounds (ls:lss) = ((0,0),(length ls, length (ls:lss)))

-- part a

theSource :: Coords
theSource = (500,0)

aoc2214 :: IO ()
aoc2214 = do 
    input <- readFile "./resources/input14test"
    -- input <- readFile "./resources/input14real"
    -- part a
    print $ translate $ splitIntoLines input
    putStrLn "\n"
    print $ pathForARock "498,4 -> 498,6 -> 496,6"

-- data Cave = Cave Rocks Source Sand Grain
translate :: [String] -> Cave 
translate ls = let rocks = []
                   source = theSource
                   sand = []
                   grain = undef
               in Cave rocks source sand grain

-- to do fill edges of path
pathForARock :: String -> [Coords]
pathForARock s =  let rawCoords = Split.splitOn " -> " s
                      corners = map (toCoords . Split.splitOn ",") rawCoords 
                  in corners

toCoords :: [String] -> Coords
toCoords (x:y:_) = (readInt x, readInt y)

-- pretty printer might be helpful this time
airChr :: Char
airChr = '.'
rockChr :: Char
rockChr = '#'
sourceChr :: Char
sourceChr = '+'
sandChr :: Char 
sandChr = 'o'
grainChar :: Char
grainChar = '~'
grainProducedChar :: Char
grainProducedChar = '*'
errorChar :: Char
errorChar = '?'


