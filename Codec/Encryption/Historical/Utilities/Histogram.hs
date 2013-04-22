module Codec.Encryption.Historical.Utilities.Histogram
  (
    Histogram,
    histogram,
    histogramDelta,
    histogramVar,
    rotate
  )
  where

-- Libraries
import Data.Maybe
import Control.Arrow
import Control.Monad

import qualified Data.Map as M

-- Main Analysis

type Histogram a = [(a, Float)]

rotate :: [(a,b)] -> [(a,b)]
rotate l = zip (drop 1 ks ++ take 1 ks) (map snd l) where ks = map fst l

histogramDelta :: (Eq a, Ord a) => Histogram a -> Histogram a -> Float
histogramDelta a b = sum $ map (**2) $ catMaybes deltas
  where
    keys   = map fst a -- Really only need one set since we're after the intersection of keys
    deltas = map (\x -> liftM2 (-) (lookup x a) (lookup x b)) keys

histogram :: (Eq a, Ord a) => [a] -> Histogram a
histogram = normalize . simpleHistogram

normalize :: Ord a => Histogram a -> Histogram a
normalize h = map (second (/total)) h where total = sum $ map snd $ h

simpleHistogram :: (Eq a, Ord a) => [a] -> Histogram a
simpleHistogram = M.toList . foldr (M.alter (Just . maybe 1 succ)) M.empty

histogramVar :: Histogram a -> Float
histogramVar h = sum $ map (**2) $ deltas
  where
    deltas = map (subtract mean) vals
    mean   = sum vals / fromIntegral (length vals)
    vals   = map snd h
