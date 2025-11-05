{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day02
       ( Input
       , filename
       , parser
       , part1
       , part1'
       , part2
       , part2'
       ) where

import Data.Attoparsec.Text
import Parsers

type Input = [(Int, Int, Int)]

filename :: String
filename = "data/Day02.txt"

parser :: Parser Input
parser = do
    xs <- listOfParser (rowOfParser (Just 'x') decimal)
    pure $ map toTriple xs

toTriple :: [Int] -> (Int, Int, Int)
toTriple (a:b:c:_) = (a, b, c)
toTriple xs        = error $ "Wrong number of parameters: " <> show xs

part1 :: Input -> Int
part1 = sum . map part1'

part1' :: (Int, Int, Int) -> Int
part1' (x,y,z) =
  let
    a = x * y
    b = x * z
    c = y * z
  in
    2 * (a + b + c) + minimum [a,b,c]

part2 :: Input -> Int
part2 = sum . map part2'

part2' :: (Int, Int, Int) -> Int
part2' (x,y,z) =
  let
    a = 2 * (x + y)
    b = 2 * (x + z)
    c = 2 * (y + z)
    d = x * y * z
  in
    minimum [a,b,c] + d