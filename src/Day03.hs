{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day03
       ( Input
       , Dirn(..)
       , filename
       , parser
       , part1
       , part2
       ) where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Set as S
import Parsers

data Dirn = N | S | E | W deriving stock (Show, Eq)

type Input = [Dirn]

filename :: String
filename = "data/Day03.txt"

parser :: Parser Input
parser = rowOfParser Nothing dirnParser

dirnParser :: Parser Dirn
dirnParser = f '^' N <|> f 'v' S <|> f '<' W <|> f '>' E
  where
    f c d = do
        _ <- char c
        pure d

part1 :: Input -> Int
part1 xs = 
  let
    ys = toCoords (0,0) xs
    zs = S.fromList ys
  in
    S.size zs

toCoords :: (Int, Int) -> Input -> [(Int, Int)]
toCoords (x,y) [] = [(x,y)]
toCoords (x,y) (N:xs) = (x,y) : toCoords (x - 1, y) xs
toCoords (x,y) (S:xs) = (x,y) : toCoords (x + 1, y) xs
toCoords (x,y) (E:xs) = (x,y) : toCoords (x, y + 1) xs
toCoords (x,y) (W:xs) = (x,y) : toCoords (x, y - 1) xs


part2 :: Input -> Int
part2 xs = 
  let 
    ys = S.fromList $ toCoords (0,0) $ everySecond xs
    zs = S.fromList $ toCoords (0,0) $ everySecond (tail xs)
  in 
    S.size (S.union ys zs)

everySecond :: [a] -> [a]
everySecond [] = []
everySecond [a] = [a]
everySecond (a:_:xs) = a : everySecond xs