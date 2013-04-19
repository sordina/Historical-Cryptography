{- | Codec.Encryption.Historical.Caesar.Test
 -   =======================================
 -
 -   Currently Checks
 -
 -   * encrypt-decrypt identity
 -   * effective encryption
 -
 -   QuickCheck version causes issues. Run with -
 -     ghci -Wall -XTemplateHaskell -XQuasiQuotes -package QuickCheck-2.5.1.1 -i. "%"
 -}

module Codec.Encryption.Historical.Caesar.Test
  (test) where

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

import Codec.Encryption.Historical.Caesar.Implementation
import Codec.Encryption.Historical.Caesar.Analysis

-- Test Items

test :: Test
test = testGroup "Caesar Cipher"
  [ testGroup "Implementation"
      [ testProperty "Identity"            prop_identity
      , testProperty "Encoded Differently" prop_different
      , testProperty "Histogram Delta"     prop_histogramDelta
      ]
  , testGroup "Analysis"
      [ testCase "Cracking the Caesar Cypher" test_crack ]
  ]

-- Test Implementation

prop_identity :: Int -> String -> Bool
prop_identity n s' = s == (caesar_decode n . caesar_encode n) s
  where s = strip s'

prop_different :: Int -> String -> Property
prop_different n s' = not (null s)
                   && mod n 26 /= 0
                  ==> caesar_encode n s /= s
  where s = strip s'

-- Test Analysis

prop_histogramDelta :: String -> Bool
prop_histogramDelta s = histogramDelta h h == 0 where h = histogram s

test_crack :: Assertion
test_crack = do
  secret <- readFile "Corpus/snow_white_abridged.txt"
  corpus <- readFile "Corpus/snow_white_abridged2.txt"

  let
    stripped_corpus = caesar_encode 0 corpus
    stripped_secret = caesar_encode 0 secret
    histo           = histogram stripped_corpus
    encrypted       = caesar_encode 12 secret -- Offset of 12 whynot?
    decrypted       = crack histo encrypted

  when (stripped_secret /= decrypted) $ assertFailure "Failed to decrypt snow_white_abridged.txt"

-- Helpers

strip :: String -> String
strip = filter (\c -> isAscii c && isAlpha c) . map toUpper
