module Day11Spec (spec) where

import qualified Data.Text as T
import Day11 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "...#......\n\
  \.......#..\n\
  \#.........\n\
  \..........\n\
  \......#...\n\
  \.#........\n\
  \.........#\n\
  \..........\n\
  \.......#..\n\
  \#...#....."

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldSatisfy` (== 374)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 10 input `shouldSatisfy` (== 1030)
      part2 100 input `shouldSatisfy` (== 8410)
