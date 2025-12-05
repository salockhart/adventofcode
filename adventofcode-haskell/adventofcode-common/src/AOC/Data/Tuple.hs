module AOC.Data.Tuple where

import Data.Bifunctor (bimap)

fromList2 :: [b] -> (b, b)
fromList2 [] = error "cannot construct a tuple from an empty list"
fromList2 [_] = error "cannot construct a tuple from a single element list"
fromList2 (a : b : _) = (a, b)

fromList3 :: [b] -> (b, b, b)
fromList3 [] = error "cannot construct a tuple from an empty list"
fromList3 [_] = error "cannot construct a tuple from a single element list"
fromList3 [_, _] = error "cannot construct a tuple from a two element list"
fromList3 (a : b : c:_) = (a, b, c)

fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) = a

sndOf3 :: (a, b, c) -> b
sndOf3 (_, b, _) = b

trdOf3 :: (a, b, c) -> c
trdOf3 (_, _, c) = c

take2Of3 :: (a, b, c) -> (a, b)
take2Of3 (a, b, _) = (a, b)

-- this is also available in https://hackage.haskell.org/package/extra
both :: (a -> b) -> (a, a) -> (b, b)
both fn = bimap fn fn
