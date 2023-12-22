module Day03 (main, part1, part2) where

import AOC (dbg, mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, neighbours8, readCoordMap)
import Control.Arrow (Arrow (second))
import Data.Char (isDigit)
import Data.List (sortOn, union)
import qualified Data.Map as M
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2023 03 part1 part2

getValidParts :: CoordMap Char -> [(Int, [(Coord, Char)])]
getValidParts coordMap =
  getValidParts'
    []
    ((sortOn (snd . fst) . M.toList) coordMap)
  where
    getSymbolNeighbours =
      filter (\(_, c) -> not (isDigit c) && c /= '.')
        . foldl union []
        . map ((`neighbours8` coordMap) . fst)
    getValidParts' partNumbers [] = partNumbers
    getValidParts' partNumbers entryList = do
      let rest = dropWhile (not . isDigit . snd) entryList
      let (number, rest') = span (isDigit . snd) rest
      let neighbours = dbg $ getSymbolNeighbours number
      let partNumber = read (map snd number) :: Int
      getValidParts'
        ((partNumber, neighbours) : partNumbers)
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
    . readCoordMap
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
    . readCoordMap
    . lines
    . T.unpack
