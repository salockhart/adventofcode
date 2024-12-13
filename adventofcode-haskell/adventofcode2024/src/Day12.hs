module Day12 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, east, neighbours4, north, northeast, northwest, readCoordMap, south, southeast, southwest, west)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 12 part1 part2

parse :: T.Text -> CoordMap Char
parse = readCoordMap . map T.unpack . T.lines . T.strip

regions :: CoordMap Char -> [S.Set Coord]
regions cm
  | cm == M.empty = []
  | otherwise =
      let (c, v) = M.findMin cm
          r = region cm (S.singleton c) v
          cm' = M.withoutKeys cm r
       in r : regions cm'

region :: CoordMap Char -> S.Set Coord -> Char -> S.Set Coord
region cm cs v = do
  let cs' =
        S.union cs $
          S.fromList $
            concatMap
              ( map fst
                  . filter ((== v) . snd)
                  . (`neighbours4` cm)
              )
              cs
  if cs' == cs
    then cs
    else region cm cs' v

perimeter :: S.Set Coord -> Int
perimeter rs =
  let cm = M.fromSet (const 'X') rs
   in sum . map ((4 -) . length . (`neighbours4` cm)) $ S.toList rs

corners :: S.Set Coord -> Int
corners rs =
  let cm = M.fromSet (const 'X') rs
   in sum . map (`corners'` cm) $ S.toList rs
  where
    corners' r cm =
      length $
        filter isCorner $
          map
            (\(a, b, c) -> (a r cm, b r cm, c r cm))
            [ (west, northwest, north),
              (north, northeast, east),
              (east, southeast, south),
              (south, southwest, west)
            ]
      where
        -- The top left of A, in
        -- _ _ _
        -- _ A B
        -- _ B B
        isCorner (Nothing, Nothing, Nothing) = True
        -- The top left of A, in
        -- A _ _
        -- _ A B
        -- _ B B
        isCorner (Nothing, Just _, Nothing) = True
        -- The top left of A, in
        -- _ A _
        -- A A B
        -- _ B B
        isCorner (Just _, Nothing, Just _) = True
        isCorner _ = False

part1 :: T.Text -> Int
part1 =
  sum
    . map (\r -> length r * perimeter r)
    . regions
    . parse

part2 :: T.Text -> Int
part2 =
  sum
    . map (\r -> length r * corners r)
    . regions
    . parse
