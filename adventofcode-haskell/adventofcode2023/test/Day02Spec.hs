{-# LANGUAGE OverloadedStrings #-}

module Day02Spec (spec) where

import Data.Text (Text, unpack)
import Day02 (part1, part2)
import Test.Hspec

input :: Text
input =
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
  \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
  \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
  \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
  \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

spec :: Spec
spec = do
  describe "Part1" $ do
    it (unpack input) $ do
      part1 input `shouldBe` 8

  describe "Part2" $ do
    it (unpack input) $ do
      part2 input `shouldBe` 2286
