{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main ) where

import System.IO

bindDouble :: [a1] -> (a1 -> a1 -> [a2]) -> [a2]
bindDouble [] _ = []
bindDouble [_] _ = []
bindDouble (x:y:[]) f = f x y
bindDouble (x:y:xs) f = (f x y) ++ ((y:xs) `bindDouble` f)

bindTriple :: [a1] -> (a1 -> a1 -> a1 -> [a2]) -> [a2]
bindTriple [] _ = []
bindTriple [_] _ = []
bindTriple (_:_:[]) _ = []
bindTriple (x:y:z:[]) f = f x y z
bindTriple (x:y:z:xs) f = (f x y z) ++ ((y:z:xs) `bindTriple` f)

filter2 :: Ord b => b -> b -> [(b, b)]
filter2 x y = if x < y then [(x ,y)] else []

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  strings <- hGetContents handle
  let nums = read <$> words strings :: [Integer]
  let group = nums `bindDouble` filter2
  let group2 = nums `bindTriple` (\x -> \y -> \z -> [x+y+z]) `bindDouble` filter2
  putStrLn $ show $ length group
  putStrLn $ show $ length group2
