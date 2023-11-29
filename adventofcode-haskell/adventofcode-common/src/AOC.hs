module AOC
  ( solveAoCDay,
    splitOn,
    groupOn,
    applyN,
    forceUnwrap,
    dbg,
    btoi,
    slice,
    chunks,
    minimumOn,
    maximumOn,
    combinations,
    median,
    parseIntoCoordMap,
    getNeighbours,
    getDiagonals,
    dbgCoordMap,
    Coord,
    CoordMap,
  )
where

import Advent (AoC (AoCInput, AoCSubmit), Part (Part1, Part2), defaultAoCOpts, mkDay_, runAoC)
import qualified Data.Foldable as Foldable
import Data.Functor ((<&>))
import Data.List (foldl', groupBy, maximumBy, minimumBy, tails)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text (unpack)
import qualified Data.Text as T
import Debug.Trace (trace)
import System.Environment (getEnv)

-- Advent API

solveAoCDay :: Integer -> Integer -> (String -> String) -> (String -> String) -> IO ()
solveAoCDay year day part1Solver part2Solver = do
  token <- getToken

  let runner = runAoC' token
  input <- runner getInput >>= handleInputError

  doSolve "Part 1: " part1Solver Part1 input runner
  doSolve "Part 2: " part2Solver Part2 input runner

  return ()
  where
    getToken = getEnv "AOC_SESSION"
    runAoC' key = runAoC (defaultAoCOpts year key)
    getInput = AoCInput (mkDay_ day)
    handleInputError = either (error . show) (return . unpack)
    getSubmit = AoCSubmit (mkDay_ day)
    doSolve label solver part input runner = do
      putStr label
      let result = solver input
      submissionResult <-
        if result `elem` invalidAoCSubmissions
          then return "N/A"
          else runner (getSubmit part result) >>= handleSubmitError <&> show
      putStrLn submissionResult
    handleSubmitError = either (error . show) (return . snd)
    invalidAoCSubmissions = ["1", "false"]

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

dbg :: Show a => a -> a
dbg x = trace (show x) x

btoi :: [Int] -> Int
btoi = sum . zipWith (\i x -> x * (2 ^ i)) [0 ..] . reverse

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
chunks n cs = [slice i (i + n - 1) cs | i <- [0 .. (length cs - n)]]

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

getNeighbours :: CoordMap a -> Coord -> CoordMap a
getNeighbours m (x, y) =
  let candidates = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
   in Map.map forceUnwrap $ Map.fromList $ filter (isJust . snd) $ map (\c -> (c, m Map.!? c)) candidates

getDiagonals :: CoordMap a -> Coord -> CoordMap a
getDiagonals m (x, y) =
  let candidates = [(x - 1, y - 1), (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1)]
   in Map.map forceUnwrap $ Map.fromList $ filter (isJust . snd) $ map (\c -> (c, m Map.!? c)) candidates

dbgCoordMap :: Show a => CoordMap a -> CoordMap a
dbgCoordMap m =
  trace
    ( concat
        [ ( concat
              [ getValue m x y ++ " "
                | x <- [minX .. maxX]
              ]
          )
            ++ "\n"
          | y <- [minY .. maxY]
        ]
    )
    m
  where
    coords = Map.keys m
    xs = map fst coords
    ys = map snd coords
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys
    getValue m x y =
      let val = m Map.!? (x, y)
       in if isJust val then show $ forceUnwrap val else " "
