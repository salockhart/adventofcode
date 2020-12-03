module Day3Spec (spec) where

import Day3 (part1, part2)
import Test.Hspec

input = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 7 for\n" ++ input) $ do
      part1 input `shouldBe` 7

  describe "Part2" $ do
    it ("returns 336 for\n" ++ input) $ do
      part2 input `shouldBe` 336
