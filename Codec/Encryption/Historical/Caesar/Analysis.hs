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

import qualified Data.Map as M

-- Main Analysis

type Histogram a = [(a, Float)]

crack :: Histogram Char -> String -> String
crack h cypher = caesar_decode offset cypher
  where
    offset     = fst $ head $ sortBy (comparing best) rotations
    best       = histogramDelta (histogram cypher) . snd
    rotations  = zip [0..25] $ iterate rotate $ sort h

rotate :: [(a,b)] -> [(a,b)]
rotate l = zip (drop 1 ks ++ take 1 ks) (map snd l) where ks = map fst l

histogramDelta :: (Eq a, Ord a) => Histogram a -> Histogram a -> Float
histogramDelta a b = result
  where
    keys   = map fst a -- Really only need one set since we're after the intersection of keys
    deltas = map (\x -> liftM2 (-) (lookup x a) (lookup x b)) keys
    result = sum $ map (**2) $ catMaybes deltas

histogram :: (Eq a, Ord a) => [a] -> Histogram a
histogram = normalize . simpleHistogram

normalize :: Ord a => Histogram a -> Histogram a
normalize h = map (second (/total)) h where total = sum $ map snd $ h

simpleHistogram :: (Eq a, Ord a) => [a] -> Histogram a
simpleHistogram = M.toList . foldr (M.alter (Just . maybe 1 succ)) M.empty
