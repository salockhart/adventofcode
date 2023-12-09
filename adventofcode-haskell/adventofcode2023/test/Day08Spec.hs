{-# LANGUAGE OverloadedStrings #-}

module Day08Spec (spec) where

import qualified Data.Text as T
import Day08 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "RL\n\
  \\n\
  \AAA = (BBB, CCC)\n\
  \BBB = (DDD, EEE)\n\
  \CCC = (ZZZ, GGG)\n\
  \DDD = (DDD, DDD)\n\
  \EEE = (EEE, EEE)\n\
  \GGG = (GGG, GGG)\n\
  \ZZZ = (ZZZ, ZZZ)"

input2 :: T.Text
input2 =
  "LLR\n\
  \\n\
  \AAA = (BBB, BBB)\n\
  \BBB = (AAA, ZZZ)\n\
  \ZZZ = (ZZZ, ZZZ)"

input3 :: T.Text
input3 =
  "LR\n\
  \\n\
  \11A = (11B, XXX)\n\
  \11B = (XXX, 11Z)\n\
  \11Z = (11B, XXX)\n\
  \22A = (22B, XXX)\n\
  \22B = (22C, 22C)\n\
  \22C = (22Z, 22Z)\n\
  \22Z = (22B, 22B)\n\
  \XXX = (XXX, XXX)"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 2)

    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 6)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input3) $ do
      part2 input3 `shouldSatisfy` (== 6)
