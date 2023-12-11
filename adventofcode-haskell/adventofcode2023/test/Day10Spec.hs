{-# LANGUAGE OverloadedStrings #-}

module Day10Spec (spec) where

import qualified Data.Text as T
import Day10 (part1, part2)
import Test.Hspec

input1 :: T.Text
input1 =
  "-L|F7\n\
  \7S-7|\n\
  \L|7||\n\
  \-L-J|\n\
  \L|-JF"

input2 :: T.Text
input2 =
  "7-F7-\n\
  \.FJ|7\n\
  \SJLL7\n\
  \|F--J\n\
  \LJ.LJ"

input3 :: T.Text
input3 =
  "...........\n\
  \.S-------7.\n\
  \.|F-----7|.\n\
  \.||.....||.\n\
  \.||.....||.\n\
  \.|L-7.F-J|.\n\
  \.|..|.|..|.\n\
  \.L--J.L--J.\n\
  \..........."

input4 :: T.Text
input4 =
  ".F----7F7F7F7F-7....\n\
  \.|F--7||||||||FJ....\n\
  \.||.FJ||||||||L7....\n\
  \FJL7L7LJLJ||LJ.L-7..\n\
  \L--J.L7...LJS7F-7L7.\n\
  \....F-J..F7FJ|L7L7L7\n\
  \....L7.F7||L7|.L7L7|\n\
  \.....|FJLJ|FJ|F7|.LJ\n\
  \....FJL-7.||.||||...\n\
  \....L---J.LJ.LJLJ..."

input5 :: T.Text
input5 =
  "FF7FSF7F7F7F7F7F---7\n\
  \L|LJ||||||||||||F--J\n\
  \FL-7LJLJ||||||LJL-77\n\
  \F--JF--7||LJLJ7F7FJ-\n\
  \L---JF-JLJ.||-FJLJJ7\n\
  \|F|F-JF---7F7-L7L|7|\n\
  \|FFJF7L7F-JF7|JL---7\n\
  \7-L-JL7||F7|L7F-7F7|\n\
  \L.L7LFJ|||||FJL7||LJ\n\
  \L7JLJL-JLJLJL--JLJ.L"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 4)

    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 8)

  describe "Part2" $ do
    it ("\n" ++ T.unpack input3) $ do
      part2 input3 `shouldSatisfy` (== 4)

    it ("\n" ++ T.unpack input4) $ do
      part2 input4 `shouldSatisfy` (== 8)

    it ("\n" ++ T.unpack input5) $ do
      part2 input5 `shouldSatisfy` (== 10)
