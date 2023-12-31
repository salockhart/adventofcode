module AOC.Data.Map where

import Data.Map (Map, empty, foldlWithKey, insertWith)

-- reverse the edges of the map
transposeM :: Ord b => Map a [b] -> Map b [a]
transposeM =
  foldlWithKey
    ( \m' k ->
        foldl
          (\m'' v -> insertWith (++) v [k] m'')
          m'
    )
    empty

-- reverse the edges of the map, applying a function to map a value to a key
transposeMWithMap :: Ord c => (b -> [c]) -> Map a b -> Map c [a]
transposeMWithMap f =
  foldlWithKey
    ( \m' k ->
        foldl
          (\m'' v -> insertWith (++) v [k] m'')
          m'
          . f
    )
    empty
