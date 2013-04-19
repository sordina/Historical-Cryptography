
module Codec.Encryption.Historical.Caesar.Test
  (test) where

import Test.Framework.Providers.QuickCheck2
import Test.Framework  (Test, testGroup)
import Codec.Encryption.Historical.Caesar.Implementation

test :: Test
test = testGroup "Caesar Cipher"
  [ testProperty "Identity" identityProperty
  ]

identityProperty :: Int -> String -> Bool
identityProperty n s = s == (caesar_decode n . caesar_encode n) s
