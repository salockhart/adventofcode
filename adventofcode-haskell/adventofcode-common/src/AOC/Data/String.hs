module AOC.Data.String where

import Data.Char (toUpper)
import qualified Data.Text as T

splitOn :: String -> String -> [String]
splitOn delim =
  map T.unpack
    . T.splitOn (T.pack delim)
    . T.pack

strip :: String -> String
strip = T.unpack . T.strip . T.pack

readHex :: String -> Integer
readHex [] = 0
readHex hxStr = hexToInt (toUpper (last hxStr)) + (16 * readHex (init hxStr))
  where
    hexToInt :: Char -> Integer
    hexToInt '0' = 0
    hexToInt '1' = 1
    hexToInt '2' = 2
    hexToInt '3' = 3
    hexToInt '4' = 4
    hexToInt '5' = 5
    hexToInt '6' = 6
    hexToInt '7' = 7
    hexToInt '8' = 8
    hexToInt '9' = 9
    hexToInt 'A' = 10
    hexToInt 'B' = 11
    hexToInt 'C' = 12
    hexToInt 'D' = 13
    hexToInt 'E' = 14
    hexToInt 'F' = 15
    hexToInt x = error ("invalid hex char (" ++ show x ++ ")")
