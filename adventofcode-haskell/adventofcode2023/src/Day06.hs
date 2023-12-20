{-# LANGUAGE OverloadedStrings #-}

module Day06 (main, part1, part2) where

import AOC (mkAoCMain)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepBy, skipSome, some)
import Text.Megaparsec.Char (char, letterChar, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)

main :: IO ()
main = mkAoCMain 2023 06 part1 part2

type Parser = Parsec Void T.Text

parseRaces :: Parser [(Int, Int)]
parseRaces = do
  [times, distances] <- parseLine `sepBy` newline
  return $ zip times distances

parseLine :: Parser [Int]
parseLine = do
  skipSome letterChar
  skipSome $ char ':'
  space
  decimal `sepBy` some (char ' ')

solve :: Int -> Int -> [Int]
solve time distance =
  [ d
    | t <- [0 .. time],
      let d = t * (time - t),
      d > distance
  ]

part1 :: T.Text -> Int
part1 =
  product
    . map (length . uncurry solve)
    . fromRight (error "couldn't parse")
    . parse parseRaces "day 06"
    . T.strip

part2 :: T.Text -> Int
part2 =
  length
    . uncurry solve
    . combineNumbers
    . fromRight (error "couldn't parse")
    . parse parseRaces "day 06"
    . T.strip
  where
    combineNumbers =
      bimap read read
        . foldl
          (\(l, r) (l', r') -> (l ++ show l', r ++ show r'))
          ("", "")
