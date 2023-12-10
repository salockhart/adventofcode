{-# LANGUAGE OverloadedStrings #-}

module Day09Spec (spec) where

import qualified Data.Text as T
import Day09 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "0 3 6 9 12 15\n\
  \1 3 6 10 15 21\n\
  \10 13 16 21 30 45"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 114)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 2)
