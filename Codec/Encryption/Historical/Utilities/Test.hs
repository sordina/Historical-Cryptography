
module Codec.Encryption.Historical.Utilities.Test (test) where

-- Testing Libraries

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework  (Test, testGroup)

-- Algoritms

import Codec.Encryption.Historical.Utilities.Histogram

-- Test Items

test :: Test
test = testGroup "Caesar Cipher"
  [ testGroup "Histogram"
      [ testProperty "Histogram Delta"           prop_histogramDelta
      , testProperty "Histogram Variance Bounds" prop_histogramVarBounds
      ]
  ]

prop_histogramDelta :: String -> Bool
prop_histogramDelta s = histogramDelta h h == 0 where h = histogram s

prop_histogramVarBounds :: String -> Property
prop_histogramVarBounds s = length s > 0
                        ==> hv >= 0 && hv <= 1  where hv = histogramVar $ histogram s
