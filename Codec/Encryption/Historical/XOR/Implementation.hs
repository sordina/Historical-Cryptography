{- | http://en.wikipedia.org/wiki/XOR_cipher -}

{-# LANGUAGE OverloadedStrings #-}

module Codec.Encryption.Historical.XOR.Implementation
  ( xor_algorithm
  , encode
  , decode
  , xor_encode
  , xor_decode
  )
where

import Data.Bits
import qualified Data.ByteString.Internal as B

data XOR = XOR { encode :: String -> String
               , decode :: String -> String }

xor_encode :: String -> String -> String
xor_encode key = encode (xor_algorithm key)

xor_decode :: String -> String -> String
xor_decode key = decode (xor_algorithm key)

xor_algorithm :: String -> XOR
xor_algorithm key = XOR enc enc
  where
    enc plain = zipWith
                (\x y -> B.w2c $ xor (B.c2w x) (B.c2w y))
                (cycle key)
                plain
