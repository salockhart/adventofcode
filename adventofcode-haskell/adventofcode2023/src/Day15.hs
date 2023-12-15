{-# LANGUAGE OverloadedStrings #-}

module Day15 (main, part1, part2) where

import AOC (solveAoCDay)
import Data.Char (ord)
import qualified Data.Foldable as F
import Data.Maybe (fromJust)
import qualified Data.Sequence as S
import qualified Data.Text as T

main :: IO ()
main = solveAoCDay 2023 15 part1 part2

hash :: [Char] -> Int
hash = hash' 0
  where
    hash' n [] = n
    hash' n (c : cs') = hash' ((n + ord c) * 17 `mod` 256) cs'

-- placeLenses :: S.Seq [([Char], Int)] -> [Char] -> S.Seq [([Char], Int)]
placeLenses :: S.Seq (S.Seq ([Char], Int)) -> [Char] -> S.Seq (S.Seq ([Char], Int))
placeLenses boxes instruction
  | last instruction == '-' = do
      let label = init instruction
      let boxNum = hash label
      S.adjust' (S.filter ((/= label) . fst)) boxNum boxes
  | otherwise = do
      let label = init $ init instruction
      let lensNum = read [last instruction]
      let boxNum = hash label
      let existingIndex = S.findIndexL ((== label) . fst) (fromJust $ boxes S.!? boxNum)
      case existingIndex of
        Just i -> S.adjust' (S.update i (label, lensNum)) boxNum boxes
        Nothing -> S.adjust' (S.|> (label, lensNum)) boxNum boxes

part1 :: T.Text -> Int
part1 =
  sum
    . map (hash . T.unpack)
    . T.splitOn ","
    . T.strip

part2 :: T.Text -> Int
part2 =
  sum
    . F.toList
    . S.mapWithIndex
      ( \i box ->
          sum $
            F.toList $
              S.mapWithIndex (\j (_, lens) -> (i + 1) * (j + 1) * lens) box
      )
    . foldl placeLenses (S.replicate 256 S.empty)
    . map T.unpack
    . T.splitOn ","
    . T.strip
