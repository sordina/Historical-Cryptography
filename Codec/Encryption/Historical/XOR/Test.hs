
module Codec.Encryption.Historical.XOR.Test (test) where

-- Normal Libraries

import Data.Char
import Control.Monad

-- Testing Libraries

import Test.QuickCheck
import Test.HUnit.Lang
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.Framework  (Test, testGroup)

-- Algoritms

import Codec.Encryption.Historical.XOR.Implementation
import Codec.Encryption.Historical.XOR.Analysis
import Codec.Encryption.Historical.Utilities.Histogram

-- Test Items

test :: Test
test = testGroup "XOR Cipher"
  [ testGroup "Implementation"
      [ testProperty "Identity"            prop_identity
      , testProperty "Encoded Differently" prop_different
      ]
  , testGroup "Analysis"
      [testCase "Cracking the XOR Cypher Key Length" test_crack_key_length
      , testCase "Cracking the XOR Cypher" test_crack
      ]
  ]

-- Test Implementation

prop_identity :: String -> String -> Property
prop_identity k s = not (null k)
                ==> s == (xor_decode k . xor_encode k) s

prop_different :: String -> String -> Property
prop_different k s  = not (null s)
                   && not (null k)
                   && s /= k
                  ==> xor_encode k s /= s

test_crack :: Assertion
test_crack = do
  secret <- readFile "Corpus/snow_white_abridged.txt"
  corpus <- readFile "Corpus/snow_white_abridged2.txt"

  let
    histo     = histogram corpus
    encrypted = xor_encode "SECRET" secret
    decrypted = crack 100 histo encrypted

  putStrLn $ take 100 $ decrypted
  when (secret /= decrypted) $ assertFailure "Failed to decrypt snow_white_abridged.txt"

test_crack_key_length :: Assertion
test_crack_key_length = do
  secret <- readFile "Corpus/snow_white_abridged.txt"
  corpus <- readFile "Corpus/snow_white_abridged2.txt"

  let
    key        = "SECRET"
    histo      = histogram corpus
    encrypted  = xor_encode key secret
    key_length = crack_key_length 100 histo encrypted

  when (key_length /= length key) $ assertFailure
                                  $ concat [ "Failed to find key length: "
                                           , show (length key)
                                           ," - Found: "
                                           ,show key_length
                                           ]
