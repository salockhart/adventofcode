module Day04
  ( main,
    part1,
    part2,
    isValidBYR,
    isValidIYR,
    isValidEYR,
    isValidHGT,
    isValidHCL,
    isValidECL,
    isValidPID,
  )
where

import AOC (splitOn)
import Data.List (isInfixOf)
import Text.Regex.PCRE ((=~))

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

hasFields :: String -> Bool
hasFields str =
  all
    (== True)
    ([id `isInfixOf` str | id <- ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]])

isValidBYR :: String -> Bool
isValidBYR str = do
  let (_, _, _, year : _) = str =~ "byr:([0-9]+)" :: (String, String, String, [String])
  let year' = read year :: Int
  year' >= 1920 && year' <= 2002

isValidIYR :: String -> Bool
isValidIYR str = do
  let (_, _, _, year : _) = str =~ "iyr:([0-9]+)" :: (String, String, String, [String])
  let year' = read year :: Int
  year' >= 2010 && year' <= 2020

isValidEYR :: String -> Bool
isValidEYR str = do
  let (_, _, _, year : _) = str =~ "eyr:([0-9]+)" :: (String, String, String, [String])
  let year' = read year :: Int
  year' >= 2020 && year' <= 2030

isValidHGT :: String -> Bool
isValidHGT str = do
  let (_, _, _, matches) = str =~ "hgt:([0-9]+)([a-z]+)" :: (String, String, String, [String])
  case matches of
    [] -> False
    [_] -> False
    [height, unit] -> do
      let height' = read height :: Int
      case unit of
        "cm" -> height' >= 150 && height' <= 193
        "in" -> height' >= 59 && height' <= 76
        _ -> False

isValidHCL :: String -> Bool
isValidHCL str = do
  let (_, _, _, matches) = str =~ "hcl:#([a-f0-9]+)" :: (String, String, String, [String])
  case matches of
    [] -> False
    [color] -> length color == 6

isValidECL :: String -> Bool
isValidECL str = do
  let (_, _, _, matches) = str =~ "ecl:([a-z]+)" :: (String, String, String, [String])
  case matches of
    [] -> False
    [color] -> color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPID :: String -> Bool
isValidPID str = do
  let (_, _, _, matches) = str =~ "pid:([0-9]+)" :: (String, String, String, [String])
  case matches of
    [] -> False
    [num] -> length num == 9

parse :: String -> [String]
parse = splitOn "\n\n"

part1 :: String -> Int
part1 =
  length
    . filter hasFields
    . parse

part2 :: String -> Int
part2 =
  length
    . filter isValidPID
    . filter isValidECL
    . filter isValidHCL
    . filter isValidHGT
    . filter isValidEYR
    . filter isValidIYR
    . filter isValidBYR
    . filter hasFields
    . parse
