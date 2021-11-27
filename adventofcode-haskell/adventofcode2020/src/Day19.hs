module Day19 (main, part1, part2) where

import AOC (splitOn)
import Data.Either (rights)
import qualified Data.IntMap as Map
import Data.List (find, foldl', isSuffixOf)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    many,
    parse,
    sepBy,
    some,
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    space1,
    spaceChar,
  )

type Parser = Parsec Void String

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parseOr :: Parser [[Int]]
parseOr = do
  parseSequence `sepBy` char '|'
  where
    parseSequence :: Parser [Int]
    parseSequence = do
      a <- many digitChar `sepBy` spaceChar
      return $ map read $ filter (/= "") a

parseRule :: Parser (Int, [[Int]])
parseRule = do
  rule <- some digitChar
  char ':'
  space1
  inner <- parseOr
  return (read rule, inner)

flatten :: [(Int, [[Int]])] -> Map.IntMap [[Int]]
flatten rules =
  foldl'
    (\m (i, rule) -> Map.insert i rule m)
    Map.empty
    $ filter (\(_, r) -> r /= [[]]) rules

parse' :: [String] -> (Int, Int, Map.IntMap [[Int]], [String])
parse' [rules, msgs] = do
  let aRule = findRuleIndex "\"a\"" $ lines rules
  let bRule = findRuleIndex "\"b\"" $ lines rules
  let rules' =
        flatten $
          rights $
            map (parse parseRule "day 19") $
              lines rules
  (aRule, bRule, rules', lines msgs)
  where
    findRuleIndex :: String -> [String] -> Int
    findRuleIndex a = maybe 0 (read . head . splitOn ":") . find (a `isSuffixOf`)

stripRule :: Int -> Int -> String -> Map.IntMap [[Int]] -> Int -> [String]
stripRule _ _ "" _ _ = []
stripRule aRule bRule s m index
  | index == aRule = [tail s | head s == 'a']
  | index == bRule = [tail s | head s == 'b']
  | otherwise = do
    let (Just r) = Map.lookup index m
    concatMap (stripRuleSeq aRule bRule s m) r

stripRuleSeq :: Int -> Int -> String -> Map.IntMap [[Int]] -> [Int] -> [String]
stripRuleSeq _ _ s _ [] = [s]
stripRuleSeq aRule bRule s m (i : is) = do
  stripped <- stripRule aRule bRule s m i
  stripRuleSeq aRule bRule stripped m is

matchesRule :: Int -> Int -> Map.IntMap [[Int]] -> Int -> String -> Bool
matchesRule aRule bRule m i s = do
  let tails = stripRule aRule bRule s m i
  "" `elem` tails

solve :: (Int, Int, Map.IntMap [[Int]], [String]) -> Int
solve (aRule, bRule, rulesMap, input) = length $ filter (matchesRule aRule bRule rulesMap 0) input

part1 :: String -> String
part1 = show . solve . parse' . splitOn "\n\n"

part2 :: String -> String
part2 = show . solve . modify . parse' . splitOn "\n\n"
  where
    modify (aRule, bRule, m, input) = do
      let m' =
            Map.insert 11 [[42, 31], [42, 11, 31]] $
              Map.insert 8 [[42], [42, 8]] m
      (aRule, bRule, m', input)
