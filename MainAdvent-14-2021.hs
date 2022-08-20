{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main ) where

import System.IO
import Data.List
import Data.Bits

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.State

splitBy delimiter = 
  foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

makeRule :: [Char] -> [ [Char] ]
makeRule rule = [ [rule!!0] ++ [rule!!1], [rule!!6] ]

makeOrigin :: [Char] -> [[Char]] 
makeOrigin origin = 
  tail $ foldr (\item -> \state -> [ [item], [item] ++ (head state)] ++ (tail state)) [ "0" ] origin

runRule :: [ [[Char]] ] -> [Char] -> [Char] -> [Char]
runRule rules item state =
  foldr (\rule -> \pair -> 
    if rule!!0 == pair then [pair!!0] ++ rule!!1 ++ [pair!!1] else pair) 
    item rules

clipHelper :: [Char] -> [Char]
clipHelper [] = []
clipHelper [x] = [x]
clipHelper (x:y:[]) = [x]
clipHelper (x:y:z:[]) = x:y:[]

clipEnds :: [[Char]] -> [[Char]]
clipEnds groups = clipHelper <$> groups

transform :: [[[Char]]] -> [[Char]] -> [[Char]]
transform rules origin = 
  foldr (\item -> \state -> [runRule rules item item] ++ state) [] origin

merge :: [[Char]] -> [Char]
merge transformed = concat $ clipEnds transformed

runAlgo :: [[[Char]]] -> [[Char]] -> [[Char]]
runAlgo rules origin = 
  foldl (\state -> \item -> makeOrigin $ merge $ transform rules state) origin [1..10]
--3950 - 644

runAlgo40 :: [[[Char]]] -> [[Char]] -> [[Char]]
runAlgo40 rules origin = 
  foldl (\state -> \item -> makeOrigin $ merge $ transform rules state) origin [1..40]

groupsOf n str =
  unfoldr (\s ->
            if length s < n
            then Nothing
            else Just (take n s, tail s)) str

frequency :: (Ord t, Eq t) => [t] -> [(t, Int)]
frequency =
  map (\s -> (head s, length s)) . group . sort

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let text = lines contents
  let origin = makeOrigin $ head text ++ ['0']
  let rulesPrep = tail $ tail $ text
  let rules = makeRule <$> rulesPrep
  let resultA = merge $ runAlgo rules origin
  let resultB = merge $ runAlgo40 rules origin
  print origin
  print $ rules
  print $ frequency resultA
  print $ frequency resultB
