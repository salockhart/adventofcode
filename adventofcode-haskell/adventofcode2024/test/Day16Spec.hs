module Day16Spec (spec) where

import qualified Data.Text as T
import Day16 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "###############\n\
  \#.......#....E#\n\
  \#.#.###.#.###.#\n\
  \#.....#.#...#.#\n\
  \#.###.#####.#.#\n\
  \#.#.#.......#.#\n\
  \#.#.#####.###.#\n\
  \#...........#.#\n\
  \###.#.#####.#.#\n\
  \#...#.....#.#.#\n\
  \#.#.#.###.#.#.#\n\
  \#.....#...#.#.#\n\
  \#.###.#.#.#.#.#\n\
  \#S..#.....#...#\n\
  \###############"

input2 :: T.Text
input2 =
  "#################\n\
  \#...#...#...#..E#\n\
  \#.#.#.#.#.#.#.#.#\n\
  \#.#.#.#...#...#.#\n\
  \#.#.#.#.###.#.#.#\n\
  \#...#.#.#.....#.#\n\
  \#.#.#.#.#.#####.#\n\
  \#.#...#.#.#.....#\n\
  \#.#.#####.#.###.#\n\
  \#.#.#.......#...#\n\
  \#.#.###.#####.###\n\
  \#.#.#...#.....#.#\n\
  \#.#.#.#####.###.#\n\
  \#.#.#.........#.#\n\
  \#.#.#.#########.#\n\
  \#S#.............#\n\
  \#################"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 7036)

    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 11048)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input1) $ do
      part2 input1 `shouldSatisfy` (== 45)

    it ("\n" ++ T.unpack input2) $ do
      part2 input2 `shouldSatisfy` (== 64)
