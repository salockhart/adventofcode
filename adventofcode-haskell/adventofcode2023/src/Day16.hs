{-# LANGUAGE OverloadedStrings #-}

module Day16 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.CoordMap (Coord, CoordMap, readCoordMap)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

type Beam = (Coord, Coord)

main :: IO ()
main = mkAoCMain 2023 16 part1 part2

minus :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
minus (xB, yB) (xA, yA) = (xB - xA, yB - yA)

plus :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
plus (xB, yB) (xA, yA) = (xB + xA, yB + yA)

solve :: S.Set Beam -> CoordMap Char -> S.Set Beam
solve = solve' S.empty

solve' :: S.Set Beam -> S.Set Beam -> CoordMap Char -> S.Set Beam
solve' history beams contraption
  | null beams = history
  | otherwise = do
      let beams' = foldl1 S.union (S.map solve'' beams) `S.difference` history
      let history' = history `S.union` beams'
      solve' history' beams' contraption
  where
    solve'' :: Beam -> S.Set Beam
    solve'' (beamFrom, beamTo) = case target of
      Nothing -> S.empty
      Just '.' -> S.fromList [(beamTo, beamTo `plus` diff)]
      Just '/' ->
        if movingHorizontally
          then
            if xDiff > 0 -- moving from left to right
              then S.fromList [(beamTo, beamTo `plus` (0, -1))] -- move up
              else S.fromList [(beamTo, beamTo `plus` (0, 1))] -- move down
          else
            if yDiff > 0 -- moving from top to bottom
              then S.fromList [(beamTo, beamTo `plus` (-1, 0))] -- move left
              else S.fromList [(beamTo, beamTo `plus` (1, 0))] -- move right
      Just '\\' ->
        if movingHorizontally
          then
            if xDiff > 0 -- moving from left to right
              then S.fromList [(beamTo, beamTo `plus` (0, 1))] -- move down
              else S.fromList [(beamTo, beamTo `plus` (0, -1))] -- move up
          else
            if yDiff > 0 -- moving from top to bottom
              then S.fromList [(beamTo, beamTo `plus` (1, 0))] -- move right
              else S.fromList [(beamTo, beamTo `plus` (-1, 0))] -- move left
      Just '|' ->
        if movingHorizontally
          then S.fromList [(beamTo, beamTo `plus` (0, -1)), (beamTo, beamTo `plus` (0, 1))] -- split up and down
          else S.fromList [(beamTo, beamTo `plus` diff)] -- pass through
      Just '-' ->
        if movingHorizontally
          then S.fromList [(beamTo, beamTo `plus` diff)] -- pass through
          else S.fromList [(beamTo, beamTo `plus` (-1, 0)), (beamTo, beamTo `plus` (1, 0))] -- move left and right
      Just x -> error ("bad character '" ++ show x ++ "'")
      where
        diff@(xDiff, yDiff) = beamTo `minus` beamFrom
        movingHorizontally = xDiff /= 0 && yDiff == 0
        target = contraption M.!? beamTo

part1 :: T.Text -> Int
part1 =
  length
    . S.map fst
    . solve (S.fromList [((-1, 0), (0, 0))])
    . readCoordMap
    . lines
    . T.unpack

part2 :: T.Text -> Int
part2 =
  maximum
    . ( \contraption ->
          map
            ( \b ->
                length $
                  S.map fst $
                    solve (S.fromList [b]) contraption
            )
            (startingBeams contraption)
      )
    . readCoordMap
    . lines
    . T.unpack
  where
    startingBeams contraption =
      concat
        [ [((minX - 1, y), (minX, y)) | y <- [minY .. maxY]],
          [((maxX + 1, y), (maxX, y)) | y <- [minY .. maxY]],
          [((x, minY - 1), (x, minY)) | x <- [minX .. maxX]],
          [((x, maxY + 1), (x, maxY)) | x <- [minX .. maxX]]
        ]
      where
        coords = M.keys contraption
        xs = map fst coords
        ys = map snd coords
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys