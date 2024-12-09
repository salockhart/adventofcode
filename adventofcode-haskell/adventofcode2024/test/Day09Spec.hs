module Day09Spec (spec) where

import qualified Data.Text as T
import Day09 (part1, part2)
import Test.Hspec

input :: T.Text
input = "2333133121414131402"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 1928)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 2858)
