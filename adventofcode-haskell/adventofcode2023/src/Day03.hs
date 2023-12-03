module Day03 (main, part1, part2) where

import AOC (Coord, CoordMap, getDiagonals, getNeighbours, parseIntoCoordMap, solveAoCDay)
import Control.Arrow (Arrow (second))
import Data.Char (isDigit)
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = solveAoCDay 2023 03 part1 part2

getValidParts :: CoordMap Char -> [(Int, [(Coord, Char)])]
getValidParts coordMap =
  getValidParts'
    []
    ((sortOn (snd . fst) . M.toList) coordMap)
  where
    getNeighbours' c =
      M.union
        (getDiagonals coordMap c)
        (getNeighbours coordMap c)
    getSymbolNeighbours =
      M.filter (\c -> not (isDigit c) && c /= '.')
        . foldl M.union M.empty
        . map (getNeighbours' . fst)
    getValidParts' partNumbers [] = partNumbers
    getValidParts' partNumbers entryList = do
      let rest = dropWhile (not . isDigit . snd) entryList
      let (number, rest') = span (isDigit . snd) rest
      let neighbours = getSymbolNeighbours number
      let partNumber = read (map snd number) :: Int
      getValidParts'
        ((partNumber, M.toList neighbours) : partNumbers)
        rest'

part1 :: T.Text -> Int
part1 =
  -- add all the parts numbers together
  sum
    . map fst
    -- find all the parts that have a symbol neighbour
    . filter (not . null . snd)
    -- get all the valid parts
    . getValidParts
    . parseIntoCoordMap
    . lines
    . T.unpack

part2 :: T.Text -> Int
part2 =
  -- multiply to get the gear ratio, sum them to get the answer
  sum
    . map product
    -- get all the gears with 2 parts
    . M.elems
    . M.filter ((== 2) . length)
    -- build a map of gear -> [partNumber...]
    . foldl (\m (partNumber, ns) -> foldl (\m' n -> M.insertWith (++) n [partNumber] m') m ns) M.empty
    -- only look at parts with "gears" aka *
    . filter (not . null . snd)
    . map (second (filter ((== '*') . snd)))
    -- get all the valid parts
    . getValidParts
    . parseIntoCoordMap
    . lines
    . T.unpack
