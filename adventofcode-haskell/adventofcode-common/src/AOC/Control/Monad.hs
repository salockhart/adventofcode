module AOC.Control.Monad where

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM p (a : as) =
  do
    test <- p a
    if test
      then return True
      else anyM p as
