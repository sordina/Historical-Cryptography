
module Codec.Encryption.Historical.Caesar.Analysis
  (
    histogram,
    histogramDelta,
    crack
  )
  where

-- Module to Analyse
import Codec.Encryption.Historical.Caesar.Implementation

-- Libraries
import Data.List
import Data.Maybe
import Data.Ord
import Control.Arrow
import Control.Monad

-- Main Analysis

-- TODO: Rotate the histogram rather than the cypher-text
-- TODO: Normalize the histograms

crack :: Histogram Char -> String -> String
crack h body = head $ sortBy (comparing best) solutions
  where
    solutions = map (flip caesar_encode body) [0..25]
    best t    = histogramDelta h (histogram t)

histogramDelta :: (Eq a, Ord a) => Histogram a -> Histogram a -> Int
histogramDelta a b = result
  where
    keys   = nub (map fst a ++ map fst b)
    deltas = map (\x -> liftM2 (-) (lookup x a) (lookup x b)) keys
    result = sum $ map (^ (2::Int)) $ catMaybes deltas

-- Helpers

type Histogram a = [(a, Int)]

-- TODO: Improve efficiency
histogram :: (Eq a, Ord a) => [a] -> Histogram a
histogram = map (head &&& length) . group . sort


