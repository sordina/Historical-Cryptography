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
import Data.ByteString.Internal

data XOR = XOR { encode :: String -> String
               , decode :: String -> String }

xor_encode :: String -> String -> String
xor_encode key = encode (xor_algorithm key)

xor_decode :: String -> String -> String
xor_decode key = decode (xor_algorithm key)

xor_algorithm :: [Char] -> XOR
xor_algorithm key = XOR enc enc
  where
    enc plain = unpackChars $ packBytes $ zipWith xor keyb plainb
      where
        keyb   = cycle $ unpackBytes $ packChars key
        plainb =         unpackBytes $ packChars plain
