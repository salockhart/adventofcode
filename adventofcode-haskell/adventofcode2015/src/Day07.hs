module Day07 (main, part1, part2) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List (find, isInfixOf, isSuffixOf)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))

main :: IO ()
main = interact (show . \input -> (part1 "a" input, part2 "a" input))

getInputLineFor :: String -> [String] -> String
getInputLineFor target inputs = do
  let input = find (\line -> ("-> " ++ target) `isSuffixOf` line) inputs
  case input of
    Nothing -> error ("no line found that provides to " ++ target)
    Just input' -> trace input' input'

solve :: String -> [String] -> Map.Map String Int -> (Int, Map.Map String Int)
solve input inputs memory
  | "AND" `isInfixOf` input = do
    let (_, _, _, lhs : rhs : _) = input =~ "(.*) AND (.*) -> .*" :: (String, String, String, [String])
    let (lhs', memory') = getValueFor lhs inputs memory
    let (rhs', memory'') = getValueFor rhs inputs memory'
    (lhs' .&. rhs', memory'')
  | "OR" `isInfixOf` input = do
    let (_, _, _, lhs : rhs : _) = input =~ "(.*) OR (.*) -> .*" :: (String, String, String, [String])
    let (lhs', memory') = getValueFor lhs inputs memory
    let (rhs', memory'') = getValueFor rhs inputs memory'
    (lhs' .|. rhs', memory'')
  | "LSHIFT" `isInfixOf` input = do
    let (_, _, _, lhs : rhs : _) = input =~ "(.*) LSHIFT (.*) -> .*" :: (String, String, String, [String])
    let (lhs', memory') = getValueFor lhs inputs memory
    let (rhs', memory'') = getValueFor rhs inputs memory'
    (lhs' `shiftL` rhs', memory'')
  | "RSHIFT" `isInfixOf` input = do
    let (_, _, _, lhs : rhs : _) = input =~ "(.*) RSHIFT (.*) -> .*" :: (String, String, String, [String])
    let (lhs', memory') = getValueFor lhs inputs memory
    let (rhs', memory'') = getValueFor rhs inputs memory'
    (lhs' `shiftR` rhs', memory'')
  | "NOT" `isInfixOf` input = do
    let (_, _, _, lhs : _) = input =~ "NOT (.*) -> .*" :: (String, String, String, [String])
    let (lhs', memory') = getValueFor lhs inputs memory
    (65535 - lhs', memory')
  | otherwise = do
    let (_, _, _, lhs : _) = input =~ "(.*) -> .*" :: (String, String, String, [String])
    let (lhs', memory') = getValueFor lhs inputs memory
    (lhs', memory')

getValueFor :: String -> [String] -> Map.Map String Int -> (Int, Map.Map String Int)
getValueFor target inputs memory =
  case memory Map.!? target of
    Just val -> trace (target ++ "=" ++ show val) (val, memory)
    Nothing -> case readMaybe target :: Maybe Int of
      Just num -> (num, memory)
      Nothing -> do
        let (val, memory') = solve (getInputLineFor target inputs) inputs memory
        trace (target ++ "=" ++ show val) (val, Map.insert target val memory')

part1 :: String -> String -> Int
part1 target input = do
  fst $ getValueFor target (lines input) Map.empty

overrideWire :: String -> Int -> String -> String
overrideWire target val line =
  if (" " ++ target) `isSuffixOf` line
    then show val ++ " -> " ++ target
    else line

part2 :: String -> String -> Int
part2 target input = do
  let overrideVal = part1 target input
  let modifiedLines = map (overrideWire "b" overrideVal) $ lines input
  fst $ getValueFor target modifiedLines Map.empty
