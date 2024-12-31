module Day20 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, manhattan, neighbours4, readCoordMap)
import AOC.Pathing.Dijkstra (dijkstra)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 20 (part1 (>= 100)) (part2 (>= 100))

parse :: T.Text -> CoordMap Char
parse = readCoordMap . lines . T.unpack

pathSegmentsWithLength :: [a] -> [((a, a), Int)]
pathSegmentsWithLength (x : xs) = zip [(x, y) | y <- xs] [1 ..] ++ pathSegmentsWithLength xs
pathSegmentsWithLength _ = []

expand :: Char -> Coord -> CoordMap Char -> [(Coord, Char)]
expand safe c cm = filter ((`elem` [safe, 'E']) . snd) $ neighbours4 c cm

racetrack :: M.Map Coord Char -> [Coord]
racetrack cm = do
  let path =
        dijkstra
          (map ((,1) . fst) . (\c -> expand '.' c cm))
          (fst $ head $ M.toList $ M.filter (== 'S') cm)
          (\c -> (cm M.! c) == 'E')
  head $ snd $ fromJust path

solve :: Int -> (Int -> Bool) -> CoordMap Char -> Int
solve n p =
  length
    . map fst
    . filter
      ( \(hnl, ogTime) ->
          let cheatTime = uncurry manhattan hnl
           in cheatTime <= n && p (ogTime - cheatTime)
      )
    . pathSegmentsWithLength
    . racetrack

part1 :: (Int -> Bool) -> T.Text -> Int
part1 p =
  solve 2 p
    . parse

part2 :: (Int -> Bool) -> T.Text -> Int
part2 p =
  solve 20 p
    . parse
