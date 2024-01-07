module Day20 (main, part1, part2) where

import AOC (mkAoCMain)
import AOC.Data.Map (transposeMWithMap)
import AOC.Data.Tuple (fromList2, fstOf3, sndOf3)
import Data.Bifunctor (first, second)
import Data.List (group, sort)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Printf (printf)

main :: IO ()
main = mkAoCMain 2023 20 part1 part2

data Module
  = Broadcaster [T.Text]
  | FlipFlop Bool [T.Text]
  | Conjunction (M.Map T.Text Bool) [T.Text]
  deriving (Eq)

instance Show Module where
  show (Broadcaster os) = printf "-> %s" (show os)
  show (FlipFlop state os) = printf "[%s] -> %s" (show state) (show os)
  show (Conjunction state os) = printf "[%s] -> %s" (show state) (show os)

outputs :: Module -> [T.Text]
outputs (Broadcaster os) = os
outputs (FlipFlop _ os) = os
outputs (Conjunction _ os) = os

parse :: T.Text -> M.Map T.Text Module
parse =
  updateConjunctions
    . M.fromList
    . map
      ( parse'
          . first head
          . fromList2
          . map (T.splitOn ", ")
          . T.splitOn " -> "
      )
    . T.lines
  where
    parse' (name, os)
      | name == "broadcaster" = (name, Broadcaster os)
      | T.head name == '%' = (T.tail name, FlipFlop False os)
      | T.head name == '&' = (T.tail name, Conjunction M.empty os)
      | otherwise = error "couldn't parse"
    updateConjunctions circuit = M.mapWithKey updateConjunction circuit
      where
        revCircuit = transposeMWithMap outputs circuit
        updateConjunction k (Conjunction _ os) = Conjunction (M.fromList $ map (,False) (revCircuit M.! k)) os
        updateConjunction _ m = m

sendPulse :: [(T.Text, Bool, T.Text)] -> M.Map T.Text Module -> (M.Map T.Text Module, [(T.Text, Bool, T.Text)])
sendPulse [] circuit = (circuit, [])
sendPulse (pulse@(pulseFrom, pulseHigh, pulseTo) : rest) circuit = do
  case circuit M.!? pulseTo of
    Nothing -> second (pulse :) $ sendPulse rest circuit
    Just n -> do
      let (node', pulses') = sendPulse' pulseHigh n
      let circuit' = M.insert pulseTo node' circuit
      second (pulse :) $ sendPulse (rest ++ pulses') circuit'
  where
    mkPulses p = map (pulseTo,p,)
    sendPulse' p n@(Broadcaster os) = (n, mkPulses p os)
    sendPulse' True n@(FlipFlop _ _) = (n, [])
    sendPulse' False (FlipFlop False os) = (FlipFlop True os, mkPulses True os)
    sendPulse' False (FlipFlop True os) = (FlipFlop False os, mkPulses False os)
    sendPulse' p (Conjunction state os) = do
      let state' = M.insert pulseFrom p state
      let allHigh = and state'
      (Conjunction state' os, mkPulses (not allHigh) os)

part1 :: T.Text -> Int
part1 =
  product
    . map length
    . group
    . sort
    . map sndOf3
    . snd
    . (!! 1000)
    . iterate
      ( \(c, ps) ->
          let (c', ps') = sendPulse [("button", False, "broadcaster")] c
           in (c', ps ++ ps')
      )
    . (,[])
    . parse

part2 :: T.Text -> Int
part2 =
  foldl1 lcm
    . M.elems
    . uncurry findWhenTargetsSendHighPulse
    . second
      ( map snd
          . iterate (sendPulse [("button", False, "broadcaster")] . fst)
          . (,[])
      )
    . withTargets
    . parse
  where
    withTargets circuit = do
      let inverse = transposeMWithMap outputs circuit
      let rxInput = inverse M.! "rx"
      let rxInputInputs = concatMap (inverse M.!) rxInput
      ((rxInputInputs, head rxInput), circuit)
    findWhenTargetsSendHighPulse :: ([T.Text], T.Text) -> [[(T.Text, Bool, T.Text)]] -> M.Map T.Text Int
    findWhenTargetsSendHighPulse (riis, ri) pulses = do
      let initial = M.fromList $ map (,maxBound :: Int) riis
      let numberedPulses = zip [0 ..] pulses
      head $
        dropWhile (\m -> maxBound `elem` M.elems m) $
          scanl
            ( \r (i, ps) ->
                M.unionWith min r $
                  M.fromList $
                    map
                      ((,i) . fstOf3)
                      ( filter
                          ( \(f, h, t) ->
                              h && f `elem` riis && t == ri
                          )
                          ps
                      )
            )
            initial
            numberedPulses
