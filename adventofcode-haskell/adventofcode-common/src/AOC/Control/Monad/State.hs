module AOC.Control.Monad.State where

import Control.Monad.State (MonadState (get), State, modify)
import qualified Data.Set as S

remember :: (Ord a) => a -> State (S.Set a) ()
remember a = modify (S.insert a)

haveSeen :: (Ord a) => a -> State (S.Set a) Bool
haveSeen a = do
  seen <- get
  return (a `S.member` seen)

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM p (a : as) =
  do
    test <- p a
    if test
      then return True
      else anyM p as