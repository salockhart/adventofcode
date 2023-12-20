{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day08 (main, part1, part2) where

import AOC (mkAoCMain)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.List (uncons)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2023 08 part1 part2

parse :: T.Text -> (String, M.Map T.Text (T.Text, T.Text))
parse =
  bimap T.unpack (M.fromList . map parseEdge . tail)
    . fromMaybe (error "found empty list for lines")
    . uncons
    . T.lines
  where
    parseEdge edge = do
      let [from, to] = T.splitOn " = " edge
      let [toL, toR] = T.splitOn ", " $ T.init $ T.tail to
      (from, (toL, toR))

traverseMap :: T.Text -> (T.Text -> Bool) -> [Char] -> M.Map T.Text (T.Text, T.Text) -> Int
traverseMap _ _ [] _ = error "empty list of directions"
traverseMap from untilTo (nextDir : directions) lrMap
  | untilTo from = 0
  | otherwise = 1 + traverseMap nextPlace untilTo directions lrMap
  where
    nextPlace = do
      let (nextL, nextR) = lrMap M.! from
      if nextDir == 'L' then nextL else nextR

part1 :: T.Text -> Int
part1 = uncurry (traverseMap "AAA" (== "ZZZ")) . first cycle . parse

part2 :: T.Text -> Int
part2 =
  foldl1 lcm
    . ( \(directions, lrMap) ->
          map
            (\from -> traverseMap from ((== 'Z') . T.last) directions lrMap)
            (filter ((== 'A') . T.last) (M.keys lrMap))
      )
    . first cycle
    . parse
