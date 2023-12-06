{-# LANGUAGE OverloadedStrings #-}

module Day06Spec (spec) where

import qualified Data.Text as T
import Day06 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "Time:      7  15   30\n\
  \Distance:  9  40  200"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 288)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 71503)
