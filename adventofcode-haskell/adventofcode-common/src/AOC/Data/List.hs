module AOC.Data.List where

import Data.List (groupBy, maximumBy, minimumBy, tails)

-- | Decompose a list into 'init' and 'last'.
--
-- * If the list is empty, returns 'Nothing'.
-- * If the list is non-empty, returns @'Just' (xs, x)@,
-- where @xs@ is the 'init'ial part of the list and @x@ is its 'last' element.
--
-- See also: https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html#v:unsnoc
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINEABLE unsnoc #-}

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on2` f)
  where
    (.*.) `on2` f' = \x -> let fx = f' x in \y -> fx .*. f' y

minimumOn :: Foldable t => Ord b => (a -> b) -> t a -> a
minimumOn f = minimumBy (\a b -> f a `compare` f b)

maximumOn :: Foldable t => Ord b => (a -> b) -> t a -> a
maximumOn f = maximumBy (\a b -> f a `compare` f b)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

chunks :: Int -> [a] -> [[a]]
chunks n cs = [slice i (i + n - 1) cs | i <- [0 .. (length cs - n)]]

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows _ [_] = []
windows n xs = take n xs : windows n (drop 1 xs)

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

mapWithPrevious :: (a -> a -> a) -> [a] -> [a]
mapWithPrevious _ [] = []
mapWithPrevious fn xs = head xs : mapWithPrevious' xs
  where
    mapWithPrevious' [] = []
    mapWithPrevious' [_] = []
    mapWithPrevious' (a : b : rest) = let b' = fn a b in b' : mapWithPrevious' (b' : rest)
