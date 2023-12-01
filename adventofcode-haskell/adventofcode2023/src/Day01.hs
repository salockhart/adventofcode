{-# LANGUAGE OverloadedStrings #-}

module Day01 (main, part1, part2) where

import AOC (solveAoCDay)
import Data.Char (isControl, isDigit)
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = solveAoCDay 2023 01 part1 part2

part1 :: Text -> Int
part1 =
  sum
    . map (\l -> read [T.head l, T.last l] :: Int)
    . T.lines
    . T.filter (\c -> isDigit c || isControl c)

part2 :: Text -> Int
part2 =
  part1
    -- some words might overlap, so we leave them on either side of the number we add
    . T.replace "nine" "nine9nine"
    . T.replace "eight" "eight8eight"
    . T.replace "seven" "seven7seven"
    . T.replace "six" "six6six"
    . T.replace "five" "five5five"
    . T.replace "four" "four4four"
    . T.replace "three" "three3three"
    . T.replace "two" "two2two"
    . T.replace "one" "one1one"
