module AOC.Pathing.Internal
  ( mkPath,
  )
where

import qualified Data.Map as Map

mkPath :: (Ord c) => Map.Map c (Maybe c) -> c -> Maybe [c]
mkPath m to = case m Map.!? to of
  Nothing -> Nothing
  Just Nothing -> Just [to]
  Just (Just prev) -> mkPath m prev >>= (Just . (to :))
