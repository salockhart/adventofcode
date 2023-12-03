{-# LANGUAGE OverloadedStrings #-}

module Day03Spec (spec) where

import qualified Data.Text as T
import Day03 (part1, part2)
import Test.Hspec

input :: T.Text
input =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598.."

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input) $ do
      part1 input `shouldBe` 4361

  describe "Part2" $ do
    it ("\n" ++ T.unpack input) $ do
      part2 input `shouldBe` 467835
