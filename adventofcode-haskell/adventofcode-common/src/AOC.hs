module AOC
  ( applyN,
    btoi,
    chunks,
    combinations,
    dbg,
    forceUnwrap,
    groupOn,
    maximumOn,
    median,
    minimumOn,
    notImplemented,
    slice,
    mkAoCMain,
    splitOn,
  )
where

import Advent (AoC (AoCInput, AoCSubmit), Part (Part1, Part2), defaultAoCOpts, mkDay_, runAoC)
import Control.Exception (Exception, throw)
import qualified Data.Foldable as Foldable
import Data.Functor ((<&>))
import Data.List (groupBy, maximumBy, minimumBy, tails)
import qualified Data.List as List
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import Debug.Trace (trace)
import System.Environment (getEnv)

-- Advent API

data AoCException = NotImplementedException deriving (Show)

instance Exception AoCException

notImplemented :: a
notImplemented = throw NotImplementedException

mkAoCMain :: Show a => Show b => Integer -> Integer -> (Text -> a) -> (Text -> b) -> IO ()
mkAoCMain year day part1Solver part2Solver = do
  token <- getToken

  let runner = runAoC' token
  input <- runner getInput >>= handleInputError

  reportAnswer Part1 (pack $ show $ part1Solver input) runner
  reportAnswer Part2 (pack $ show $ part2Solver input) runner

  return ()
  where
    getToken = getEnv "AOC_SESSION"
    runAoC' key = runAoC (defaultAoCOpts year key)
    getInput = AoCInput (mkDay_ day)
    handleInputError = either (error . show) return
    getSubmit = AoCSubmit (mkDay_ day)
    reportAnswer part output runner = do
      putStr (show part)
      putStr ": "
      putStr (unpack output)
      putStr " ("
      submissionResult <- runner (getSubmit part (unpack output)) >>= handleSubmitError <&> show
      putStr submissionResult
      putStrLn ")"
    handleSubmitError = either (error . show) (return . snd)

-- String operations

splitOn :: String -> String -> [String]
splitOn delim =
  map unpack
    . Text.splitOn (pack delim)
    . pack

-- Utils

applyN :: Int -> (a -> a) -> a -> a
applyN n f = Foldable.foldr (.) id (List.replicate n f)

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
