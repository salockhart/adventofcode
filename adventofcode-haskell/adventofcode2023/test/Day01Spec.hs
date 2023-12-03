{-# LANGUAGE OverloadedStrings #-}

module Day01Spec (spec) where

import Data.Text (Text, unpack)
import Day01 (part1, part2)
import Test.Hspec (Spec, describe, it, shouldBe)

input1 :: Text
input1 =
  "1abc2\n\
  \pqr3stu8vwx\n\
  \a1b2c3d4e5f\n\
  \treb7uchet"

input2 :: Text
input2 =
  "two1nine\n\
  \eightwothree\n\
  \abcone2threexyz\n\
  \xtwone3four\n\
  \4nineeightseven2\n\
  \zoneight234\n\
  \7pqrstsixteen"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ unpack input1) $ do
      part1 input1 `shouldBe` 142

  describe "Part2" $ do
    it ("\n" ++ unpack input2) $ do
      part2 input2 `shouldBe` 281
