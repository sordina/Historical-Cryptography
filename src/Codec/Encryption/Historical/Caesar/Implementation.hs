{- | The Caesar Cypher
 -   =================
 -
 - http://en.wikipedia.org/wiki/Caesar_cipher
 -
 - Works as a pair of maps from the 26 letters of the English alphabet to an
 - offset rotation of these letters. Work has been done to preserve spaces
 - as well as to automatically capitalize input.
 -
 - For example:
 -
 - Plain:  ABCDEFGHIJKLMNOPQRSTUVWXYZ
 - Cipher: DEFGHIJKLMNOPQRSTUVWXYZABC
 -
 - encode hello -> KHOOR
 - decode KHOOR -> HELLO
 -
 - Note that this cypher is not symetrical. A string encoded twice is not decoded.
 - However, after a certain number of encodings, a string will be decoded.
 -}

module Codec.Encryption.Historical.Caesar.Implementation
  ( caesar
  , encode
  , decode
  , caesar_encode
  , caesar_decode
  )
where

-- import Data.List
import Data.Maybe
import Data.Char

data CeasarInstance = CeasarInstance {
  encode :: String -> String,
  decode :: String -> String
}

caesar :: Int -> CeasarInstance
caesar offset = CeasarInstance (mkLookup alphabet rotated) (mkLookup rotated alphabet)
  where
    rotated :: String
    rotated = rotate offset alphabet

    mkLookup :: String -> String -> String -> String
    mkLookup source destination = catMaybes . map (flip lookup table . toUpper)
      where
        table :: [(Char,Char)]
        table = zip (' ':source) (' ':destination)

caesar_encode :: Int -> String -> String
caesar_encode = encode . caesar

caesar_decode :: Int -> String -> String
caesar_decode = decode . caesar

-- Potential Library Functions

alphabet :: String
alphabet = ['A'..'Z']

alphalen :: Int
alphalen = length alphabet

rotate :: Integral n => n -> [a] -> [a]
rotate offset list = (drop n list) ++ (take n list)
  where n = fromIntegral offset `mod` alphalen
