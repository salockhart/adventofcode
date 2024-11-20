module Day21Spec (spec) where

import qualified Data.Text as T
import Day21 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "...........\n\
  \.....###.#.\n\
  \.###.##..#.\n\
  \..#.#...#..\n\
  \....#.#....\n\
  \.##..S####.\n\
  \.##..#...#.\n\
  \.......##..\n\
  \.##.#.####.\n\
  \.##..##.##.\n\
  \..........."

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 6 input `shouldSatisfy` const False -- 16
  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 6 input `shouldSatisfy` const False -- 16
    it ("\n" ++ T.unpack input) $ do
      part2 10 input `shouldSatisfy` const False -- 50
    it ("\n" ++ T.unpack input) $ do
      part2 50 input `shouldSatisfy` const False -- 1594
    it ("\n" ++ T.unpack input) $ do
      part2 100 input `shouldSatisfy` const False -- 6536
    it ("\n" ++ T.unpack input) $ do
      part2 500 input `shouldSatisfy` const False -- 167004
    it ("\n" ++ T.unpack input) $ do
      part2 1000 input `shouldSatisfy` const False -- 668697
    it ("\n" ++ T.unpack input) $ do
      part2 5000 input `shouldSatisfy` const False -- 16733044
