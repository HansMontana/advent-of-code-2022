module Aoc2207 (aoc2207) where

import Lib
import qualified Data.List.Extra as List
-- import qualified Data.Bifunctor as Bifunctor
-- import qualified Data.Char as Char
-- import qualified Data.List.Split as Split
-- import qualified Data.Set as Set
-- import qualified Data.Tree as Tree

type Path = [String]

cdCom :: String
cdCom = "$ cd "

-- Part a

cd :: Path -> String -> Path
cd _ "/" = ["/"]
cd path ".." = tail path
cd path dir = dir:path


-- FIXME: Debug till working, alternatively you can probably also use foldl to implement this
{- foldPreservingInterims :: ([a] -> a -> [a]) -> [[a]] -> [[a]]
foldPreservingInterims _ [] = []
foldPreservingInterims f (x:y:ls) = reverse $ foldPreservingInterims' [f x y] f ls
    where foldPreservingInterims' :: [a] -> (a -> b -> a) -> [b] -> [a]
          foldPreservingInterims' acc _ [] = acc
          foldPreservingInterims' (a:acc) f (l:ls) = foldPreservingInterims' (f a l : acc) f ls -}

-- ["/","a","e","..","..","d"]
solveA :: [[Char]] -> Path
solveA input = foldl cd [] $ map (drop (length cdCom)) $ filter (List.isPrefixOf cdCom) input

-- Part b

aoc2207 :: IO ()
aoc2207 = do 
    input <- readFile "./resources/input7test"
    -- input <- readFile "./resources/input7real"
    -- part a
    print $ prettyPath $ solveA $ splitIntoLines input
{-     printList $ foldPreservingInterims (++) ["1", "2", "3", "4", "5"] -}

-- To the lib!

join :: [a] -> [[a]] -> [a]
join delimiter = foldr1 (\x y -> x ++ delimiter ++ y)

-- pretty printers
prettyPath :: [String] -> String
prettyPath = join " > " . reverse