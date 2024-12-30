module Day19Spec (spec) where

import qualified Data.Text as T
import Day19 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "r, wr, b, g, bwu, rb, gb, br\n\
  \\n\
  \brwrr\n\
  \bggr\n\
  \gbbr\n\
  \rrbgbr\n\
  \ubwu\n\
  \bwurrg\n\
  \brgr\n\
  \bbrgwb"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 6)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 16)
