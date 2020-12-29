{-# LANGUAGE TupleSections #-}

module Day24 (main, part1, part2) where

import AOC (uniques)
import Data.Either (rights)
import Data.List (foldl', group, partition, sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec (Parsec, choice, many, parse)
import Text.Megaparsec.Char (string)

type Parser = Parsec Void String

type AxialCoord = (Int, Int)

type TileMap = Map.Map AxialCoord Int

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

tileInDirection :: AxialCoord -> String -> AxialCoord
tileInDirection (x, y) "e" = (x + 1, y)
tileInDirection (x, y) "se" = (x, y + 1)
tileInDirection (x, y) "sw" = (x - 1, y + 1)
tileInDirection (x, y) "w" = (x - 1, y)
tileInDirection (x, y) "nw" = (x, y - 1)
tileInDirection (x, y) "ne" = (x + 1, y - 1)

adjacents :: AxialCoord -> [AxialCoord]
adjacents coord = map (tileInDirection coord) ["e", "se", "sw", "w", "nw", "ne"]

initialize :: String -> [AxialCoord]
initialize =
  map head
    . filter ((== 1) . (`mod` 2) . length)
    . group
    . sort
    . map (findCoord (0, 0))
    . rights
    . map (parse parseDirections "day 24")
    . lines
  where
    findCoord :: AxialCoord -> [String] -> AxialCoord
    findCoord coord [] = coord
    findCoord (x, y) (a : rest) = findCoord (tileInDirection (x, y) a) rest
    parseDirections :: Parser [String]
    parseDirections =
      many
        ( choice
            [ string "e",
              string "se",
              string "sw",
              string "w",
              string "nw",
              string "ne"
            ]
        )

buildMap :: [AxialCoord] -> TileMap
buildMap = Map.fromList . map (,1)

execute :: Int -> TileMap -> TileMap
execute 0 tiles = tiles
execute round tiles = do
  let focusedTiles =
        uniques $
          concatMap (\c -> c : adjacents c) $
            Map.keys tiles
  let tiles' =
        Map.filter (== 1) $
          foldl' (\ts ft -> Map.insert ft (apply ft) ts) tiles focusedTiles
  execute (round - 1) tiles'
  where
    get :: TileMap -> AxialCoord -> Int
    get m = fromMaybe 0 . (m Map.!?)
    apply :: AxialCoord -> Int
    apply coord = do
      let me = tiles `get` coord
      let adjs = map (tiles `get`) $ adjacents coord
      let (_, blacks) = partition (== 0) adjs
      case me of
        1 -> if null blacks || length blacks > 2 then 0 else 1
        0 -> if length blacks == 2 then 1 else 0

part1 :: String -> String
part1 =
  show
    . length
    . initialize

part2 :: String -> String
part2 =
  show
    . length
    . execute 100
    . buildMap
    . initialize
