module Codec.Encryption.Historical.XOR.Analysis
  ( crack
  , crack_key_length
  )
  where

-- Module to Analyse
import Codec.Encryption.Historical.XOR.Implementation
import Codec.Encryption.Historical.Utilities.Histogram
import Data.Char
import Data.Ord
import Data.List
import Data.List.Split

-- TODO: Take advantage of some of the properties of XOR to crack this better
crack :: Int -> Histogram Char -> String -> String
crack mkl h cypher = xor_decode key cypher
  where
    key     = map (crackPart h) chopped
    chopped = transpose $ chunksOf klen cypher
    klen    = crack_key_length mkl h cypher

crackPart :: Histogram Char -> String -> Char
crackPart h cypher = chr $ head $ sortBy (comparing best) solutions
  where
    best :: Int -> Float
    best n = histogramDelta h (histogram (xor_encode [chr n] cypher))

    solutions :: [Int]
    solutions = [0..255]

crack_key_length :: Int -> Histogram a -> String -> Int
crack_key_length keyLen h s = head $ sortBy (comparing best) [1..keyLen]
  where
    hv        = histogramVar h
    best    n = (abs (hv - compute n), n)
    compute n = average
              $ map (histogramVar . histogram)
              $ transpose
              $ chunksOf n s

average :: [Float] -> Float
average l = sum l / fromIntegral (length l)
