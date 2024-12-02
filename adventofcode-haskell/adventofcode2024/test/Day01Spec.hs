module Day01Spec (spec) where

import qualified Data.Text as T
import Day01 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "3   4\n\
  \4   3\n\
  \2   5\n\
  \1   3\n\
  \3   9\n\
  \3   3"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 11)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 31)
