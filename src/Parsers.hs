module Parsers where

import Data.Attoparsec.Text

listOfIntsParser :: Parser [Int]
listOfIntsParser = listOfParser decimal

listOfParser :: Parser a -> Parser [a]
listOfParser p = many1' $ do
    x <- p 
    option () endOfLine
    pure x

rowOfIntsParser :: Parser [Int]
rowOfIntsParser = rowOfParser (Just ',') decimal

rowOfParser :: Maybe Char -> Parser a -> Parser [a]
rowOfParser Nothing    p = many1' p
rowOfParser (Just sep) p = do
    xs <- many1' $ do
        x <- p
        _ <- option sep (char sep)
        pure x
    pure xs

charToDigit :: Char -> Int
charToDigit '0' = 0
charToDigit '1' = 1
charToDigit '2' = 2
charToDigit '3' = 3
charToDigit '4' = 4
charToDigit '5' = 5
charToDigit '6' = 6
charToDigit '7' = 7
charToDigit '8' = 8
charToDigit '9' = 9
charToDigit _  = -1