module Day23 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.List (combinations)
import Data.Function ((&))
import Data.List (intercalate, sort, sortOn)
import qualified Data.Map as M
import Data.Ord (Down (Down))
import qualified Data.Set as S
import qualified Data.Text as T

main :: IO ()
main = mkAoCMain 2024 23 part1 part2

parse :: T.Text -> M.Map String (S.Set String)
parse =
  foldl combine M.empty
    . map (map T.unpack . T.splitOn (T.pack "-"))
    . T.lines
  where
    combine m [a, b] =
      M.insertWith S.union a (S.singleton b) m
        & M.insertWith S.union b (S.singleton a)
    combine _ _ = error "not implemented"

solve :: (Ord a) => ([S.Set a] -> [S.Set a]) -> M.Map a (S.Set a) -> [S.Set a]
solve t m =
  ( filter isInterconnected
      . t
      . concatMap
        ( map S.fromList
            . combinations
            . S.toList
            . uncurry S.insert
        )
      . M.assocs
  )
    m
  where
    isInterconnected cs =
      all ((cs `S.isSubsetOf`) . (\c -> S.insert c (m M.! c))) cs

part1 :: T.Text -> Int
part1 =
  length
    . S.fromList
    . map sort
    . filter (any ((== 't') . head))
    . map S.toList
    . solve (filter ((== 3) . length))
    . parse

part2 :: T.Text -> String
part2 =
  intercalate ","
    . sort
    . S.toList
    . head
    . solve (sortOn (Down . length))
    . parse
