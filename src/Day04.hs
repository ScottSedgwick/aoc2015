{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day04
       ( Input
       , filename
       , parser
       , part1
       , part2
       ) where

import Data.Attoparsec.Text
import Data.Hash.MD5
import Data.List
import Debug.Trace
import Text.Printf
import Parsers

type Input = String

filename :: String
filename = "data/Day04.txt"

parser :: Parser Input
parser = rowOfParser Nothing anyChar

part1 :: Input -> Int
part1 = part1' 0

part1' :: Int -> Input -> Int
part1' p x =
  let
    s = printf "%s%0.5d" x p
    m = md5s (Str s)
  in
    if isPrefixOf "00000" m
    then trace s $ trace (show m) $ p
    else part1' (p + 1) x

part2 :: Input -> Int
part2 = part2' 0

part2' :: Int -> Input -> Int
part2' p x =
  let
    s = printf "%s%0.5d" x p
    m = md5s (Str s)
  in
    if isPrefixOf "000000" m
    then trace s $ trace (show m) $ p
    else part2' (p + 1) x