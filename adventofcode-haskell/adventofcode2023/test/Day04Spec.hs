{-# LANGUAGE OverloadedStrings #-}

module Day04Spec (spec) where

import qualified Data.Text as T
import Day04 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
  \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
  \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
  \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
  \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
  \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldBe` 13

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldBe` 30
