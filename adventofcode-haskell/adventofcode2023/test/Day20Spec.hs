{-# LANGUAGE OverloadedStrings #-}

module Day20Spec (spec) where

import qualified Data.Text as T
import Day20 (part1)
import Test.Hspec

input1 :: T.Text
input1 =
  "broadcaster -> a, b, c\n\
  \%a -> b\n\
  \%b -> c\n\
  \%c -> inv\n\
  \&inv -> a"

input2 :: T.Text
input2 =
  "broadcaster -> a\n\
  \%a -> inv, con\n\
  \&inv -> b\n\
  \%b -> con\n\
  \&con -> output"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("\n" ++ T.unpack input1) $ do
      part1 input1 `shouldSatisfy` (== 32000000)

    it ("\n" ++ T.unpack input2) $ do
      part1 input2 `shouldSatisfy` (== 11687500)
