module AOC (splitOn) where

import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn delim =
  map T.unpack
    . T.splitOn (T.pack delim)
    . T.pack