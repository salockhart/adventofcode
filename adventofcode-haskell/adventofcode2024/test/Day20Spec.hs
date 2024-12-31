module Day20Spec (spec) where

import qualified Data.Text as T
import Day20 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "###############\n\
  \#...#...#.....#\n\
  \#.#.#.#.#.###.#\n\
  \#S#...#.#.#...#\n\
  \#######.#.#.###\n\
  \#######.#.#...#\n\
  \#######.#.###.#\n\
  \###..E#...#...#\n\
  \###.#######.###\n\
  \#...###...#...#\n\
  \#.#####.#.###.#\n\
  \#.#...#.#.#...#\n\
  \#.#.#.#.#.#.###\n\
  \#...#...#...###\n\
  \###############"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n(== 2)\n" ++ T.unpack input) $ do
      part1 (== 2) input `shouldSatisfy` (== 14)
    it ("\n(== 4)\n" ++ T.unpack input) $ do
      part1 (== 4) input `shouldSatisfy` (== 14)
    it ("\n(== 6)\n" ++ T.unpack input) $ do
      part1 (== 6) input `shouldSatisfy` (== 2)
    it ("\n(== 8)\n" ++ T.unpack input) $ do
      part1 (== 8) input `shouldSatisfy` (== 4)
    it ("\n(== 10)\n" ++ T.unpack input) $ do
      part1 (== 10) input `shouldSatisfy` (== 2)
    it ("\n(== 12)\n" ++ T.unpack input) $ do
      part1 (== 12) input `shouldSatisfy` (== 3)
    it ("\n(== 20)\n" ++ T.unpack input) $ do
      part1 (== 20) input `shouldSatisfy` (== 1)
    it ("\n(== 36)\n" ++ T.unpack input) $ do
      part1 (== 36) input `shouldSatisfy` (== 1)
    it ("\n(== 38)\n" ++ T.unpack input) $ do
      part1 (== 38) input `shouldSatisfy` (== 1)
    it ("\n(== 40)\n" ++ T.unpack input) $ do
      part1 (== 40) input `shouldSatisfy` (== 1)
    it ("\n(== 64)\n" ++ T.unpack input) $ do
      part1 (== 64) input `shouldSatisfy` (== 1)

  describe "Part2" $ do
    it ("\n(== 50)\n" ++ T.unpack input) $ do
      part2 (== 50) input `shouldSatisfy` (== 32)
    it ("\n(== 52)\n" ++ T.unpack input) $ do
      part2 (== 52) input `shouldSatisfy` (== 31)
    it ("\n(== 54)\n" ++ T.unpack input) $ do
      part2 (== 54) input `shouldSatisfy` (== 29)
    it ("\n(== 56)\n" ++ T.unpack input) $ do
      part2 (== 56) input `shouldSatisfy` (== 39)
    it ("\n(== 58)\n" ++ T.unpack input) $ do
      part2 (== 58) input `shouldSatisfy` (== 25)
    it ("\n(== 60)\n" ++ T.unpack input) $ do
      part2 (== 60) input `shouldSatisfy` (== 23)
    it ("\n(== 62)\n" ++ T.unpack input) $ do
      part2 (== 62) input `shouldSatisfy` (== 20)
    it ("\n(== 64)\n" ++ T.unpack input) $ do
      part2 (== 64) input `shouldSatisfy` (== 19)
    it ("\n(== 66)\n" ++ T.unpack input) $ do
      part2 (== 66) input `shouldSatisfy` (== 12)
    it ("\n(== 68)\n" ++ T.unpack input) $ do
      part2 (== 68) input `shouldSatisfy` (== 14)
    it ("\n(== 70)\n" ++ T.unpack input) $ do
      part2 (== 70) input `shouldSatisfy` (== 12)
    it ("\n(== 72)\n" ++ T.unpack input) $ do
      part2 (== 72) input `shouldSatisfy` (== 22)
    it ("\n(== 74)\n" ++ T.unpack input) $ do
      part2 (== 74) input `shouldSatisfy` (== 4)
    it ("\n(== 76)\n" ++ T.unpack input) $ do
      part2 (== 76) input `shouldSatisfy` (== 3)
