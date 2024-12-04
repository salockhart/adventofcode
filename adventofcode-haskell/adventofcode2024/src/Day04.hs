module Day04 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (CoordMap, east, north, northeast, northwest, readCoordMap, south, southeast, southwest, west)
import AOC.Data.Map (transposeM)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 04 part1 part2

parse :: T.Text -> CoordMap Char
parse =
  readCoordMap
    . map T.unpack
    . T.splitOn "\n"

part1 :: T.Text -> Int
part1 =
  length
    . filter isJust
    . solve
    . parse
  where
    solve cm = do
      let tcm = transposeM $ M.map (: []) cm
      let starts = tcm M.! 'X'
      let dirs =
            [ north,
              south,
              east,
              west,
              northeast,
              northwest,
              southeast,
              southwest
            ]
      concatMap (\s -> map (findWord cm s) dirs) starts

    findWord cm coord dir = do
      (mc, m) <- dir coord cm
      (ac, a) <- dir mc cm
      (sc, s) <- dir ac cm
      if m == 'M' && a == 'A' && s == 'S'
        then return (coord, mc, ac, sc)
        else Nothing

part2 :: T.Text -> Int
part2 =
  length
    . filter isJust
    . solve
    . parse
  where
    solve cm = do
      let tcm = transposeM $ M.map (: []) cm
      let starts = tcm M.! 'A'
      map (findCross cm) starts

    findCross cm coord = do
      (nwc, nw) <- northwest coord cm
      (nec, ne) <- northeast coord cm
      (sec, se) <- southeast coord cm
      (swc, sw) <- southwest coord cm
      if ([nw, se] == ['M', 'S'] || [nw, se] == ['S', 'M'])
        && ([ne, sw] == ['M', 'S'] || [ne, sw] == ['S', 'M'])
        then return (coord, nwc, nec, sec, swc)
        else Nothing
