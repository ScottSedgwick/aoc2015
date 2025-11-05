{- |
Copyright: (c) 2025 Scott Sedgwick
SPDX-License-Identifier: MIT
Maintainer: Scott Sedgwick <scott.sedgwick@gmail.com>

See README for more info
-}

module Day05
       ( Input
       , filename
       , parser
       , part1
       , part2
       ) where

import Data.Attoparsec.Text
import Data.List
import Parsers

type Input = [String]

filename :: String
filename = "data/Day05.txt"

parser :: Parser Input
parser = listOfParser letters

part1 :: Input -> Int
part1 = length . filter isNice

isNice :: String -> Bool
isNice s = threeVowels s && doubleLetter s && (not (hasForbidden s))

threeVowels :: String -> Bool
threeVowels s = length (filter isVowel s) >= 3

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel _   = False

doubleLetter :: String -> Bool
doubleLetter (a:b:xs) = (a == b) || doubleLetter (b:xs)
doubleLetter _ = False

hasForbidden :: String -> Bool
hasForbidden s = isInfixOf "ab" s
              || isInfixOf "cd" s
              || isInfixOf "pq" s
              || isInfixOf "xy" s 

part2 :: Input -> Int
part2 = length . filter isNice2
    
isNice2 :: String -> Bool
isNice2 s = hasTwoDouble s && repeatWithMiddle s

hasTwoDouble :: String -> Bool
hasTwoDouble (a:b:xs) = isInfixOf [a,b] xs || hasTwoDouble (b:xs)
hasTwoDouble _ = False

repeatWithMiddle :: String -> Bool
repeatWithMiddle (a:b:c:xs) = (a == c) || repeatWithMiddle (b:c:xs)
repeatWithMiddle _ = False