module Day04Spec (spec) where

import qualified Data.Text as T
import Day04 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "MMMSXXMASM\n\
  \MSAMXMSMSA\n\
  \AMXSXMAAMM\n\
  \MSAMASMSMX\n\
  \XMASAMXAMM\n\
  \XXAMMXXAMA\n\
  \SMSMSASXSS\n\
  \SAXAMASAAA\n\
  \MAMMMXMMMM\n\
  \MXMXAXMASX"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 18)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldSatisfy` (== 9)
