module Day08 (main, part1, part2) where

import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

main :: IO ()
main = interact (show . \input -> (part1 input, part2 input))

readNum :: String -> Int
readNum line = read ((if '+' `elem` line then drop 5 else drop 4) line)

executeLine :: (Int, Int) -> String -> (Int, Int)
executeLine (acc, ip) line
  | "acc " `isPrefixOf` line = do
    let acc' = acc + readNum line
    (acc', ip + 1)
  | "jmp " `isPrefixOf` line = do
    let ip' = ip + readNum line
    (acc, ip')
  | "nop " `isPrefixOf` line = (acc, ip + 1)

execute :: (Int, Int) -> [String] -> [(Int, Int)]
execute (acc, ip) program = do
  if ip == length program
    then [(acc, ip)]
    else do
      let line = program !! ip
      let (acc', ip') = executeLine (acc, ip) line
      (acc, ip) : execute (acc', ip') program

-- Modified from https://stackoverflow.com/a/28755555
takeUntilDuplicate :: [(Int, Int)] -> [(Int, Int)]
takeUntilDuplicate = helper Set.empty []
  where
    helper _ seen [] = seen
    helper seenSet seen (x : xs)
      | snd x `Set.member` seenSet = seen
      | otherwise = helper (Set.insert (snd x) seenSet) (seen ++ [x]) xs

part1 :: String -> Int
part1 = fst . last . takeUntilDuplicate . execute (0, 0) . lines

swapInstruction :: String -> String
swapInstruction inst
  | "jmp " `isPrefixOf` inst = "nop " ++ drop 4 inst
  | "nop " `isPrefixOf` inst = "jmp " ++ drop 4 inst

buildVariants :: [String] -> [[String]]
buildVariants lines = do
  let editableLines = [i | i <- [0 .. length lines - 1], "jmp" `isPrefixOf` (lines !! i) || "nop" `isPrefixOf` (lines !! i)]
  [[if j == i then swapInstruction (lines !! j) else lines !! j | j <- [0 .. length lines - 1]] | i <- editableLines]

part2 :: String -> Int
part2 str = do
  let strs = lines str
  let variants = buildVariants strs
  fst $ fromMaybe (0, 0) $ find (\(_, ip) -> ip == length strs) $ map (last . takeUntilDuplicate . execute (0, 0)) variants
