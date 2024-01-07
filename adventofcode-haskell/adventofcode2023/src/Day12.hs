module Day12 (main, part1, part2) where

import AOC (mkAoCMain)
import Data.Bifunctor (bimap)
import Data.Function.Memoize (memoize2)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2023 12 part1 part2

parse :: T.Text -> [(String, [Int])]
parse =
  map
    ( ( \xs ->
          ( T.unpack $ head xs,
            map ((read :: String -> Int) . T.unpack) $ T.splitOn "," (xs !! 1)
          )
      )
        . T.splitOn " "
    )
    . T.lines

checkArrangements :: [Char] -> [Int] -> Int
checkArrangements = memoize2 checkArrangements'
  where
    next = checkArrangements
    -- if we get to the end of our groups...
    checkArrangements' xs []
      -- and we still have '#'s left, no match
      | '#' `elem` xs = 0
      -- otherwise, we can collapse the rest to '.'s. match!
      | otherwise = 1
    -- if we get to the end of our records first, no match
    checkArrangements' [] _ = 0
    checkArrangements' records@(nextCharacter : restCharacters) groups@(nextGroup : restGroups)
      | nextCharacter == '.' = dot
      | nextCharacter == '#' = pound
      | nextCharacter == '?' = dot + pound
      | otherwise = error "found unknown character"
      where
        dot = next restCharacters groups
        (chunk, afterChunk) = splitAt nextGroup records
        pound
          -- not enough elements remaining, no match
          | length chunk /= nextGroup = 0
          -- if the next character after this chunk is '#', we don't have the gap we need
          | listToMaybe afterChunk == Just '#' = 0
          -- if this chunk has gaps, fail early
          | '.' `elem` chunk = 0
          -- otherwise, consume the chunk
          | otherwise = next (drop 1 afterChunk) restGroups

part1 :: T.Text -> Int
part1 =
  sum
    . map (uncurry checkArrangements)
    . parse

part2 :: T.Text -> Int
part2 =
  sum
    . map
      ( uncurry checkArrangements
          . bimap (intercalate "?" . replicate 5) (concat . replicate 5)
      )
    . parse
