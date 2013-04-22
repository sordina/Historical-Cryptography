module Codec.Encryption.Historical.Caesar.Analysis
  (
    crack
  )
  where

-- Module to Analyse
import Codec.Encryption.Historical.Caesar.Implementation
import Codec.Encryption.Historical.Utilities.Histogram

-- Libraries
import Data.List
import Data.Ord

-- Main Analysis

crack :: Histogram Char -> String -> String
crack h cypher = caesar_decode offset cypher
  where
    offset     = fst $ head $ sortBy (comparing best) rotations
    best       = histogramDelta (histogram cypher) . snd
    rotations  = zip [0..25] $ iterate rotate $ sort h
