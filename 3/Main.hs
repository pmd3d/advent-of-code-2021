{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main ) where

import System.IO
import Data.List
import Data.Bits

transformBit :: Char -> Bool
transformBit c = if c == '1' then True else False

convertToDecimal :: Integer -> Bool -> Integer
convertToDecimal num flag = num `shiftL` 1 + (if flag then 1 else 0)

part2 :: [[Bool]] -> (Int -> Int -> Bool) -> [[Bool]]
part2 xs predicate =
  foldl (\nums iteration -> 
          let columns = transpose nums in
          let subTotal = length nums in
            if subTotal == 1 
              then 
                nums 
              else 
                let digit = (predicate subTotal) $ length $ filter id $ columns !! iteration in
                  filter (\n -> n !! iteration == digit) nums
        ) xs [0..11]

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let binaryNums = (\w -> transformBit <$> w) <$> (words contents)
  let total = length binaryNums
  let columns = transpose binaryNums
  let num1bits = (> (div total 2)) <$> length <$> filter id <$> columns
  let num2bits = not <$> num1bits
  let num1 = foldl convertToDecimal 0 num1bits
  let num2 = foldl convertToDecimal 0 num2bits
  print num1
  print num2
  print (num1 * num2)
  let num1bbits = head $ part2 binaryNums (\subTotal x -> if x > (div subTotal 2) || ((0 == mod subTotal 2) && x == (div subTotal 2)) then True else False)
  let num2bbits = head $ part2 binaryNums (\subTotal x -> if (subTotal `mod` 2 == 1) && x <= (div subTotal 2) then True else False)
  let num3 = foldl convertToDecimal 0 num1bbits
  let num4 = foldl convertToDecimal 0 num2bbits
  print num3
  print num4
  print (num3 * num4)
--2777
--966
--2682582
--2781
--919
--2555739