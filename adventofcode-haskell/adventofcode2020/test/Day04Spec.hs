module Day04Spec (spec) where

import Day04 (isValidBYR, isValidECL, isValidEYR, isValidHCL, isValidHGT, isValidIYR, isValidPID, part1, part2)
import Test.Hspec

input1 =
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
  \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
  \\n\
  \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
  \hcl:#cfa07d byr:1929\n\
  \\n\
  \hcl:#ae17e1 iyr:2013\n\
  \eyr:2024\n\
  \ecl:brn pid:760753108 byr:1931\n\
  \hgt:179cm\n\
  \\n\
  \hcl:#cfa07d eyr:2025 pid:166559648\n\
  \iyr:2011 ecl:brn hgt:59in"

input2 =
  "eyr:1972 cid:100\n\
  \hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
  \\n\
  \iyr:2019\n\
  \hcl:#602927 eyr:1967 hgt:170cm\n\
  \ecl:grn pid:012533040 byr:1946\n\
  \\n\
  \hcl:dab227 iyr:2012\n\
  \ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
  \\n\
  \hgt:59cm ecl:zzz\n\
  \eyr:2038 hcl:74454a iyr:2023\n\
  \pid:3556412378 byr:2007"

input3 =
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
  \hcl:#623a2f\n\
  \\n\
  \eyr:2029 ecl:blu cid:129 byr:1989\n\
  \iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
  \\n\
  \hcl:#888785\n\
  \hgt:164cm byr:2001 iyr:2015 cid:88\n\
  \pid:545766238 ecl:hzl\n\
  \eyr:2022\n\
  \\n\
  \iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

spec :: Spec
spec = do
  describe "Part1" $ do
    it ("returns 2 for " ++ input1) $ do
      part1 input1 `shouldBe` 2

  describe "Part2" $ do
    describe "byr validation" $ do
      it "returns False for byr:20" $ do
        isValidBYR "byr:20" `shouldBe` False
      it "returns False for byr:1919" $ do
        isValidBYR "byr:1919" `shouldBe` False
      it "returns True for byr:1920" $ do
        isValidBYR "byr:1920" `shouldBe` True
      it "returns True for byr:2002" $ do
        isValidBYR "byr:2002" `shouldBe` True
      it "returns False for byr:2003" $ do
        isValidBYR "byr:2003" `shouldBe` False
      it "returns False for byr:20030" $ do
        isValidBYR "byr:2003" `shouldBe` False

    describe "iyr validation" $ do
      it "returns False for iyr:20" $ do
        isValidIYR "iyr:20" `shouldBe` False
      it "returns False for iyr:2009" $ do
        isValidIYR "iyr:2009" `shouldBe` False
      it "returns True for iyr:2010" $ do
        isValidIYR "iyr:2010" `shouldBe` True
      it "returns True for iyr:2020" $ do
        isValidIYR "iyr:2020" `shouldBe` True
      it "returns False for iyr:2021" $ do
        isValidIYR "iyr:2021" `shouldBe` False
      it "returns False for iyr:20210" $ do
        isValidIYR "iyr:20210" `shouldBe` False

    describe "eyr validation" $ do
      it "returns False for eyr:20" $ do
        isValidEYR "eyr:20" `shouldBe` False
      it "returns False for eyr:2019" $ do
        isValidEYR "eyr:2019" `shouldBe` False
      it "returns True for eyr:2020" $ do
        isValidEYR "eyr:2020" `shouldBe` True
      it "returns True for eyr:2030" $ do
        isValidEYR "eyr:2030" `shouldBe` True
      it "returns False for eyr:2031" $ do
        isValidEYR "eyr:2031" `shouldBe` False
      it "returns False for eyr:200000" $ do
        isValidEYR "eyr:200000" `shouldBe` False

    describe "hgt validation" $ do
      describe "cm" $ do
        it "returns False for hgt:149cm" $ do
          isValidHGT "hgt:149cm" `shouldBe` False
        it "returns True for hgt:150cm" $ do
          isValidHGT "hgt:150cm" `shouldBe` True
        it "returns True for hgt:193cm" $ do
          isValidHGT "hgt:193cm" `shouldBe` True
        it "returns False for hgt:194cm" $ do
          isValidHGT "hgt:194cm" `shouldBe` False
        it "returns False for hgt:193cms" $ do
          isValidHGT "hgt:193cms" `shouldBe` False
      describe "in" $ do
        it "returns False for hgt:58in" $ do
          isValidHGT "hgt:58in" `shouldBe` False
        it "returns True for hgt:59in" $ do
          isValidHGT "hgt:59in" `shouldBe` True
        it "returns True for hgt:76in" $ do
          isValidHGT "hgt:76in" `shouldBe` True
        it "returns False for hgt:77in" $ do
          isValidHGT "hgt:77in" `shouldBe` False
        it "returns False for hgt:76ins" $ do
          isValidHGT "hgt:76ins" `shouldBe` False
      describe "no unit" $ do
        it "returns False for hgt:149" $ do
          isValidHGT "hgt:149" `shouldBe` False
        it "returns False for hgt:150" $ do
          isValidHGT "hgt:150" `shouldBe` False
        it "returns False for hgt:193" $ do
          isValidHGT "hgt:193" `shouldBe` False
        it "returns False for hgt:194" $ do
          isValidHGT "hgt:194" `shouldBe` False
        it "returns False for hgt:58" $ do
          isValidHGT "hgt:58" `shouldBe` False
        it "returns False for hgt:59" $ do
          isValidHGT "hgt:59" `shouldBe` False
        it "returns False for hgt:76" $ do
          isValidHGT "hgt:76" `shouldBe` False
        it "returns False for hgt:77" $ do
          isValidHGT "hgt:77" `shouldBe` False

    describe "hcl validation" $ do
      it "returns True for hcl:#123abc" $ do
        isValidHCL "hcl:#123abc" `shouldBe` True
      it "returns False for hcl:#123abz" $ do
        isValidHCL "hcl:#123abz" `shouldBe` False
      it "returns False for hcl:123abc" $ do
        isValidHCL "hcl:123abc" `shouldBe` False
      it "returns False for hcl:123" $ do
        isValidHCL "hcl:123" `shouldBe` False
      it "returns False for hcl:#123abc123" $ do
        isValidHCL "hcl:#123abc123" `shouldBe` False

    describe "ecl validation" $ do
      it "returns True for ecl:brn" $ do
        isValidECL "ecl:brn" `shouldBe` True
      it "returns False for ecl:brns" $ do
        isValidECL "ecl:brns" `shouldBe` False
      it "returns False for ecl:br" $ do
        isValidECL "ecl:br" `shouldBe` False
      it "returns False for ecl:wat" $ do
        isValidECL "ecl:wat" `shouldBe` False

    describe "pid validation" $ do
      it "returns True for pid:000000001" $ do
        isValidPID "pid:000000001" `shouldBe` True
      it "returns False for pid:0123456789" $ do
        isValidPID "pid:0123456789" `shouldBe` False

    it ("returns 0 for " ++ input2) $ do
      part2 input2 `shouldBe` 0
    it ("returns 4 for " ++ input3) $ do
      part2 input3 `shouldBe` 4
