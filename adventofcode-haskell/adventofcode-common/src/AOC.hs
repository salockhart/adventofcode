module AOC
  ( splitOn,
    groupOn,
    applyN,
    forceUnwrap,
    slice,
    chunks,
    minimumOn,
    maximumOn,
    combinations,
    median,
    parseIntoCoordMap,
    Coord,
    CoordMap,
  )
where

import qualified Data.Foldable as Foldable
import Data.List (foldl', groupBy, maximumBy, minimumBy, tails)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T

-- String operations

splitOn :: String -> String -> [String]
splitOn delim =
  map T.unpack
    . T.splitOn (T.pack delim)
    . T.pack

-- Utils

identity :: a -> a
identity x = x

applyN :: Int -> (a -> a) -> a -> a
applyN n f = Foldable.foldr (.) identity (List.replicate n f)

forceUnwrap :: Maybe p -> p
forceUnwrap (Just x) = x
forceUnwrap Nothing = error "cannot unwrap"

-- List operations

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on2` f)
  where
    (.*.) `on2` f = \x -> let fx = f x in \y -> fx .*. f y

minimumOn :: Foldable t => Ord b => (a -> b) -> t a -> a
minimumOn f = minimumBy (\a b -> f a `compare` f b)

maximumOn :: Foldable t => Ord b => (a -> b) -> t a -> a
maximumOn f = maximumBy (\a b -> f a `compare` f b)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

chunks :: Int -> [a] -> [[a]]
chunks n cs = [slice i (i + n -1) cs | i <- [0 .. (length cs - n)]]

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations xs = [] : concat [map (x :) $ combinations xs' | (x : xs') <- tails xs]

median :: Integral a => [a] -> Maybe a
median xs
  | null xs = Nothing
  | odd len = Just $ xs !! mid
  | otherwise = Just $ (xs !! (mid - 1) + xs !! mid) `quot` 2
  where
    len = length xs
    mid = len `quot` 2

-- Map operations

type Coord = (Int, Int)

type CoordMap a = Map.Map Coord a

parseIntoCoordMap :: [[a]] -> CoordMap a
parseIntoCoordMap =
  foldl'
    ( \m (y, line) ->
        Map.union
          m
          ( foldl'
              (\m' (x, char) -> Map.insert (x, y) char m')
              Map.empty
              $ zip [0 ..] line
          )
    )
    Map.empty
    . zip [0 ..]
