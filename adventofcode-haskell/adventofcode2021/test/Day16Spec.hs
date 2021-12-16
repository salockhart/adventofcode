module Day16Spec (spec) where

import Day16 (part1, part2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Part1" $ do
    it "D2FE28" $ do
      part1 "D2FE28" `shouldBe` "6"
    it "8A004A801A8002F478" $ do
      part1 "8A004A801A8002F478" `shouldBe` "16"
    it "620080001611562C8802118E34" $ do
      part1 "620080001611562C8802118E34" `shouldBe` "12"
    it "C0015000016115A2E0802F182340" $ do
      part1 "C0015000016115A2E0802F182340" `shouldBe` "23"
    it "A0016C880162017C3686B18A3D4780" $ do
      part1 "A0016C880162017C3686B18A3D4780" `shouldBe` "31"
