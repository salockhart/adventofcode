module Day12Spec (spec) where

import qualified Data.Text as T
import Day12 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "AAAA\n\
  \BBCD\n\
  \BBCC\n\
  \EEEC"

input2 :: T.Text
input2 =
  "OOOOO\n\
  \OXOXO\n\
  \OOOOO\n\
  \OXOXO\n\
  \OOOOO"

input3 :: T.Text
input3 =
  "EEEEE\n\
  \EXXXX\n\
  \EEEEE\n\
  \EXXXX\n\
  \EEEEE"

input4 :: T.Text
input4 =
  "AAAAAA\n\
  \AAABBA\n\
  \AAABBA\n\
  \ABBAAA\n\
  \ABBAAA\n\
  \AAAAAA"

input5 :: T.Text
input5 =
  "RRRRIICCFF\n\
  \RRRRIICCCF\n\
  \VVRRRCCFFF\n\
  \VVRCCCJFFF\n\
  \VVVVCJJCFE\n\
  \VVIVCCJJEE\n\
  \VVIIICJJEE\n\
  \MIIIIIJJEE\n\
  \MIIISIJEEE\n\
  \MMMISSJEEE"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 140)

    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 772)

    it ("\n" ++ T.unpack input5) $ do
      part1 input5 `shouldSatisfy` (== 1930)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input1) $ do
      part2 input1 `shouldSatisfy` (== 80)

    it ("\n" ++ T.unpack input2) $ do
      part2 input2 `shouldSatisfy` (== 436)

    it ("\n" ++ T.unpack input3) $ do
      part2 input3 `shouldSatisfy` (== 236)

    it ("\n" ++ T.unpack input4) $ do
      part2 input4 `shouldSatisfy` (== 368)

    it ("\n" ++ T.unpack input5) $ do
      part2 input5 `shouldSatisfy` (== 1206)
