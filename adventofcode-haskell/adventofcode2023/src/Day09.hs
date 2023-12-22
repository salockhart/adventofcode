{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day09 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.List (chunks)
import Data.Either (fromRight)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepBy, some)
import Text.Megaparsec.Char (char, newline, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)

main :: IO ()
main = mkAoCMain 2023 09 part1 part2

parsePatterns :: Parsec Void T.Text [[Int]]
parsePatterns = (signed space decimal `sepBy` some (char ' ')) `sepBy` newline

diffs :: [Int] -> [Int]
diffs = map (\[a, b] -> b - a) . chunks 2

getNextValue :: [Int] -> Int
getNextValue xs
  | all (== 0) xs = 0
  | otherwise = last xs + getNextValue (diffs xs)

getPrevValue :: [Int] -> Int
getPrevValue xs
  | all (== 0) xs = 0
  | otherwise = head xs - getPrevValue (diffs xs)

part1 :: T.Text -> Int
part1 =
  sum
    . map getNextValue
    . fromRight (error "couldn't parse")
    . parse parsePatterns "day 09"
    . T.strip

part2 :: T.Text -> Int
part2 =
  sum
    . map getPrevValue
    . fromRight (error "couldn't parse")
    . parse parsePatterns "day 09"
    . T.strip
