module Codec.Encryption.Historical.XOR.Analysis
  ( crack
  , crack_key_length
  )
  where

-- Module to Analyse
import Codec.Encryption.Historical.XOR.Implementation
import Codec.Encryption.Historical.Utilities.Histogram
import Data.Ord
import Data.List
import Data.List.Split
import Control.Arrow
import qualified Data.ByteString.Internal as B

-- TODO: Take advantage of some of the properties of XOR to crack this better
crack :: Int -> Histogram Char -> String -> String
crack mkl h cypher = xor_decode key cypher
  where
    key     = map (crackPart h) chopped
    chopped = transpose $ chunksOf klen cypher
    klen    = crack_key_length mkl h cypher

crackPart :: Histogram Char -> String -> Char
crackPart h cypher = fst $ head $ sortBy (comparing best) goodSolutions
  where
    best :: (Char, Histogram Char) -> Float
    best = histogramDelta h . snd

    goodSolutions :: [(Char, Histogram Char)]
    goodSolutions = filter ((>20) . length . snd) solutions

    solutions :: [(Char, Histogram Char)]
    solutions = map ((id &&& histogram . singletonDecode cypher) . B.w2c) [minBound..maxBound]

singletonDecode :: String -> Char -> String
singletonDecode cypher c = xor_decode [c] cypher

crack_key_length :: Int -> Histogram a -> String -> Int
crack_key_length keyLen h s = head $ sortBy (comparing best) [1..keyLen]
  where
    hv        = histogramVar h
    best :: Int -> (Float, Int)
    best    n = (abs (hv - compute n), n)
    compute :: Int -> Float
    compute n = average
              $ map (histogramVar . histogram)
              $ transpose
              $ chunksOf n s

average :: [Float] -> Float
average l = sum l / fromIntegral (length l)
