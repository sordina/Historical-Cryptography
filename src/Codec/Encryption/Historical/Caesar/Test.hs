
module Codec.Encryption.Historical.Caesar.Test
  (test) where

import Data.Char

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework  (Test, testGroup)
import Codec.Encryption.Historical.Caesar.Implementation

test :: Test
test = testGroup "Caesar Cipher"
  [ testProperty "Identity"            prop_identity
  , testProperty "Encoded Differently" prop_different
  ]

prop_identity :: Int -> String -> Bool
prop_identity n s' = s == (caesar_decode n . caesar_encode n) s
  where s = strip s'

prop_different :: Int -> String -> Property
prop_different n s' = not (null s)
                   && mod n 26 /= 0
                  ==> caesar_encode n s /= s
  where s = strip s'

strip :: String -> String
strip = filter (\c -> isAscii c && isAlpha c) . map toUpper
