module AOC.Pathing.Internal
  ( mkPath,
    mkPaths,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S

mkPath :: (Ord c) => M.Map c (Maybe c) -> c -> Maybe [c]
mkPath m to = case m M.!? to of
  Nothing -> Nothing
  Just Nothing -> Just [to]
  Just (Just prev) -> mkPath m prev >>= (Just . (to :))

mkPaths :: (Ord c) => M.Map c (Maybe (S.Set c)) -> c -> [[c]]
mkPaths m to = case m M.!? to of
  Nothing -> []
  Just Nothing -> [[to]]
  Just (Just prevs) -> map (to :) $ concatMap (\p -> map (p :) $ mkPaths m p) prevs
