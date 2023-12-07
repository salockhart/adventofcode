{-# LANGUAGE OverloadedStrings #-}

module Day07Spec (spec) where

import qualified Data.Text as T
import Day07 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "32T3K 765\n\
  \T55J5 684\n\
  \KK677 28\n\
  \KTJJT 220\n\
  \QQQJA 483"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 6440)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 5905)
