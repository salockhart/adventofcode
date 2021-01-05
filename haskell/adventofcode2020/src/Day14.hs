module Day14 (main, part1, part2) where

import Data.Bits (Bits (setBit), clearBit)
import Data.List (foldl', isPrefixOf)
import qualified Data.Map as Map
import Text.Regex.PCRE ((=~))

type Memory = Map.Map Int Int

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

replace' :: Eq b => b -> b -> [b] -> [b]
replace' a b = map (\x -> if a == x then b else x)

parseMask :: String -> String
parseMask = drop 7

parseAssign :: String -> (Int, Int)
parseAssign inst = do
  let (_, _, _, address : value : _) = inst =~ "mem\\[([0-9]+)\\] = ([0-9]+)" :: (String, String, String, [String])
  (read address, read value)

applyMask :: [Char] -> Int -> Int
applyMask mask num =
  foldl'
    (\b a -> if snd a == '1' then b `setBit` fst a else b `clearBit` fst a)
    num
    [x | x <- zip [0 ..] $ reverse mask, snd x /= 'X']

execute :: String -> Memory -> [String] -> Memory
execute _ memory [] = memory
execute mask memory (inst : rest) = do
  if "mask" `isPrefixOf` inst
    then execute (parseMask inst) memory rest
    else do
      let (address, value) = parseAssign inst
      let memory' = Map.insert address (applyMask mask value) memory
      execute mask memory' rest

getMasks :: [Char] -> [[Char]]
getMasks [] = [[]]
getMasks (a : rest) = do
  let rest' = getMasks rest
  if a /= 'X'
    then map (a :) rest'
    else map ('0' :) rest' ++ map ('1' :) rest'

execute' :: [String] -> Memory -> [String] -> Memory
execute' _ memory [] = memory
execute' masks memory (inst : rest) = do
  if "mask" `isPrefixOf` inst
    then execute' (map (replace' 'Z' 'X') $ getMasks $ replace' '0' 'Z' $ parseMask inst) memory rest
    else do
      let (address, value) = parseAssign inst
      let addresses = map (`applyMask` address) masks
      let memory' = foldl' (\mem address -> Map.insert address value mem) memory addresses
      execute' masks memory' rest

part1 :: String -> String
part1 = show . sum . map snd . Map.toList . execute "" Map.empty . lines

part2 :: String -> String
part2 = show . sum . map snd . Map.toList . execute' [[]] Map.empty . lines
