{-# LANGUAGE OverloadedStrings #-}

module Day02 (main, part1, part2) where

import AOC (solveAoCDay)
import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parse, sepBy)
import Text.Megaparsec.Char (digitChar, letterChar, space, string)

main :: IO ()
main = solveAoCDay 2023 02 part1 part2

type Parser = Parsec Void Text

parseGame :: Parser (Int, [(Int, Int, Int)])
parseGame = do
  _ <- string "Game "
  gameId <- many digitChar
  _ <- string ": "
  pulls <- parsePull `sepBy` string "; "
  return (read gameId :: Int, pulls)

parsePull :: Parser (Int, Int, Int)
parsePull = do
  amounts <- parseAmount `sepBy` string ", "
  return $ foldl1 combinePulls amounts
  where
    combinePulls (r, g, b) (r', g', b') = (r + r', g + g', b + b')

parseAmount :: Parser (Int, Int, Int)
parseAmount = do
  n <- many digitChar
  _ <- space
  colour <- many letterChar
  return $ case colour of
    "red" -> (read n, 0, 0)
    "green" -> (0, read n, 0)
    "blue" -> (0, 0, read n)
    _ -> (0, 0, 0)

part1 :: Text -> Int
part1 =
  sum
    . map fst
    . filter (\(_, pulls) -> all isValidPull pulls)
    . rights
    . map (parse parseGame "day 02")
    . T.lines
  where
    isValidPull (r, g, b) = r <= 12 && g <= 13 && b <= 14

part2 :: Text -> Int
part2 =
  sum
    . map ((\(r, g, b) -> r * g * b) . (\(_, pulls) -> foldl1 getMinNumberOfDice pulls))
    . rights
    . map (parse parseGame "day 02")
    . T.lines
  where
    getMinNumberOfDice (r, g, b) (r', g', b') = (max r r', max g g', max b b')
