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

import Data.Char
import Control.Monad

import Test.QuickCheck
import Test.HUnit.Lang
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.Framework  (Test, testGroup)

import Codec.Encryption.Historical.Caesar.Implementation
import Codec.Encryption.Historical.Caesar.Analysis

-- Notes:
--   import Test.HUnit hiding (Test)
--   testGroup "cases"      $ zipWith testCase     numbering $ [] ,

-- Test List

test :: Test
test = testGroup "Caesar Cipher"
  [ testProperty "Identity"            prop_identity
  , testProperty "Encoded Differently" prop_different
  , testProperty "Histogram Delta"     prop_histogramDelta
  , testCase     "Cracking the Cypher" test_crack
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
  contents <- readFile "Corpus/snow_white_abridged.txt"

  let
    stripped  = caesar_encode 0 contents
    histo     = histogram stripped
    encrypted = caesar_encode 12 stripped
    decrypted = crack histo encrypted

  when (stripped /= decrypted) $ assertFailure "Failed to decrypt snow_white_abridged.txt"

-- Helpers

strip :: String -> String
strip = filter (\c -> isAscii c && isAlpha c) . map toUpper