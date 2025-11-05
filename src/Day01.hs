{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day01
       ( Input
       , filename
       , parser
       , part1
       , part2
       ) where

import Data.Attoparsec.Text

type Input = String

filename :: String
filename = "data/Day01.txt"

parser :: Parser Input
parser = many1' anyChar

part1 :: Input -> Int
part1 xs = 
  let
    ys = filter (== '(') xs
    zs = filter (== ')') xs
  in 
    length ys - length zs

part2 :: Input -> Int
part2 = part2' 1 0

part2' :: Int -> Int -> Input -> Int
part2' index (-1)  _      = index - 1
part2' _     _     []     = -1
part2' index level (x:xs) =
  if (x == '(')
    then part2' (index + 1) (level + 1) xs
    else part2' (index + 1) (level - 1) xs