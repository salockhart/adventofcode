{-# LANGUAGE TupleSections #-}

module Day17 (main, part1, part2) where

import AOC (dbg)
import qualified Data.Foldable as Map
import qualified Data.IntMap as Map
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

data Rect = Rect
  { leftEdge :: Int,
    topEdge :: Int,
    rightEdge :: Int,
    bottomEdge :: Int
  }
  deriving (Show)

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

parse :: [Char] -> Rect
parse =
  ( \((x1 : x2 : _) : (y1 : y2 : _) : _) ->
      Rect
        { leftEdge = min x1 x2,
          topEdge = max y1 y2,
          rightEdge = max x1 x2,
          bottomEdge = min y1 y2
        }
  )
    . map (map read . splitOn ".." . drop 2)
    . splitOn ", "
    . drop 13

seqSum :: Integral a => a -> a
seqSum x = (x * (x + 1)) `quot` 2

viableXS :: Rect -> Map.IntMap [Int]
viableXS rect =
  Map.fromListWith (++) $
    concatMap
      ( \v ->
          map (\(v, t, d) -> (t, [v])) $
            takeWithin minX maxX $
              [(v, t, distance v t) | t <- [1 .. 1000]]
      )
      [0 .. maxX]
  where
    takeWithin a b = takeWhile (\(v, t, d) -> d <= b) . dropWhile (\(v, t, d) -> d < a)
    distance v t = sum $ take t [v, v - 1 .. 0]
    minX = leftEdge rect
    maxX = rightEdge rect

viableYS :: Rect -> Map.IntMap [Int]
viableYS rect =
  Map.fromListWith (++) $
    concatMap
      ( \v ->
          map (\(v, t, d) -> (t, [v])) $
            takeWithin minY maxY $
              [(v, t, distance v t) | t <- [1 .. 1000]]
      )
      [maxY .. - maxY]
  where
    takeWithin a b = takeWhile (\(v, t, d) -> d >= b) . dropWhile (\(v, t, d) -> d > a)
    distance v t = sum $ take t [v, v - 1 ..]
    minY = topEdge rect
    maxY = bottomEdge rect

part1 :: String -> String
part1 =
  show
    . seqSum
    . maximum
    . concat
    . Map.elems
    . viableYS
    . parse

part2 :: String -> String
part2 =
  show
    -- . sum
    -- . Map.elems
    -- . (\(xs, ys) -> Map.mapWithKey (\t yv -> length yv * length (fromMaybe [] (xs Map.!? t))) ys)
    . length
    . nub
    . concat
    . Map.elems
    . (\(xs, ys) -> Map.mapWithKey (\t yvs -> let xvs = xs Map.! t in concatMap (\yv -> map (,yv) xvs) yvs) ys)
    . (\r -> (viableXS r, viableYS r))
    . parse
