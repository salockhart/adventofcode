module Day05Spec (spec) where

import qualified Data.Text as T
import Day05 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "3-5\n\
  \10-14\n\
  \16-20\n\
  \12-18\n\
  \\n\
  \1\n\
  \5\n\
  \8\n\
  \11\n\
  \17\n\
  \32"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 3)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 14)
