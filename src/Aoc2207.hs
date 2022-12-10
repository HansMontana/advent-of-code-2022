module Aoc2207 (aoc2207) where

import Lib
import qualified Data.List.Extra as List

import qualified Data.Map as Map

type FolderName = String
type Path = [FolderName]
type FileSize = Int
type FoldersWithSize = Map.Map Path FileSize
type State = (FoldersWithSize, Path)
type FileName = String

data Line = Ls | Cd FolderName | Folder FolderName | File FileSize FileName deriving (Eq, Show)

cdCom :: String
cdCom = "$ cd "
lsCom :: String
lsCom = "$ ls"
dirSignifier :: String
dirSignifier = "dir "

translateLine :: String -> Line
translateLine s
    | lsCom == s = Ls
    | List.isPrefixOf cdCom s = Cd $ drop (length cdCom) s
    | List.isPrefixOf dirSignifier s = Folder $ drop (length dirSignifier) s
    | otherwise = let [sizeString, fileName] = splitIntoWords s
                  in File (readInt sizeString) fileName

translate :: [String] -> [Line]
translate = map translateLine

-- Part a:
-- Assumptions: no use of cd into nonexistent directories (not checked on input), 
--              no use of ls on a path already read (checked on input)

cd :: Path -> String -> Path
cd _ "/" = ["/"]
cd path ".." = tail path
cd path dir = dir:path
                                       
step :: State -> Line -> State
step state Ls = state
step (sizeMap, path) (Cd folder) = (sizeMap, cd path folder)
step state (Folder _) = state -- ignore read folders, see assumptions at the top
step (sizeMap, path) (File size _) = let ancestors = filter (/= []) $ List.tails path
                                         newMap = insertWithList (+) ancestors size sizeMap
                                     in (newMap, path)


-- first result is root, tail is subdirs
determineFileSizesOfFolders :: [Line] -> [FileSize]
determineFileSizesOfFolders input = Map.elems $ fst $ foldl step (Map.empty, []) input

maximumSizeToConsider :: Int
maximumSizeToConsider = 100000

solveA :: [Line] -> Int
solveA input = sum $ filter (<= maximumSizeToConsider) $ determineFileSizesOfFolders input

-- Part b

totalSpace :: Int
totalSpace = 70000000
neededSpace :: Int
neededSpace = 30000000

-- Case not modelled here: already enought space!
solveB :: [Line] -> Int
solveB input = let (usedSpace : folderSizes) = determineFileSizesOfFolders input
                   freeSpace = totalSpace - usedSpace
                   spaceToBeFreed = neededSpace - freeSpace
               in minimum $ filter (>= spaceToBeFreed) folderSizes

aoc2207 :: IO ()
aoc2207 = do 
    -- input <- readFile "./resources/input7test"
    input <- readFile "./resources/input7real"
    -- part a
    print $ solveA $ translate $ splitIntoLines input
    -- part b
    print $ solveB $ translate $ splitIntoLines input

-- pretty printer
{- prettyPath :: [String] -> String
prettyPath = join " > " . reverse -}