module AOC (splitOn, uniques, groupOn, applyN, slice) where

import qualified Data.Foldable as Foldable
import Data.List (groupBy)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn delim =
  map T.unpack
    . T.splitOn (T.pack delim)
    . T.pack

uniques :: Ord a => [a] -> [a]
uniques = Set.toList . Set.fromList

identity :: a -> a
identity x = x

applyN :: Int -> (a -> a) -> a -> a
applyN n f = Foldable.foldr (.) identity (List.replicate n f)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on2` f)
  where
    (.*.) `on2` f = \x -> let fx = f x in \y -> fx .*. f y

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
