{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main ) where

import System.IO

bindDouble :: [a1] -> (a1 -> a1 -> [a2]) -> [a2]
bindDouble (x:y:[]) f = f x y
bindDouble (x:y:xs) f = (f x y) ++ (xs `bindDouble` f)
bindDouble [] _ = []
bindDouble [_] _ = []

data Command = Command { horizontal :: Integer, depth :: Integer } deriving Show

pairAction :: String -> String -> [Command]
pairAction commandStr numStr = 
  let
    amount = read numStr :: Integer 
  in
    case commandStr of
      "forward" -> [ Command { horizontal = amount, depth = 0 } ]
      "down" -> [ Command { horizontal = 0, depth = amount } ]
      "up" -> [ Command { horizontal = 0, depth = (-amount) } ]  
      _ ->  error "bad input"


accumulate :: (Integer, Integer) -> Command -> (Integer, Integer)
accumulate (h, d) command =
  let Command { horizontal = h2, depth = d2 } = command in 
  (h + h2, d + d2)

--(1909,655)
--1250395

accumulateWithAim :: (Integer, Integer, Integer) -> Command -> (Integer, Integer, Integer)
accumulateWithAim (h, d, aim) command =
  let Command { horizontal = h2, depth = d2 } = command in 
  (h + h2, d + h2 * aim, aim + d2 )

--[1909,760194]
--1451210346

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let step1 = (words contents) `bindDouble` pairAction
  print step1
  let step2 = foldl accumulate (0,0) step1
  print step2
  print (fst(step2) * snd(step2))
  let (first,second,_) = foldl accumulateWithAim (0,0,0) step1
  print [first, second]
  print (first * second)
