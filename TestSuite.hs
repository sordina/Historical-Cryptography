module Main (main) where

-- | Run quickly with `make test`

-- Framework
import Test.Framework                       (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Module Tests
import qualified Codec.Encryption.Historical.Caesar.Test as Caesar

-- Main
main :: IO ()
main = defaultMain tests

-- Documentation and Sanity Checking
sanityCheck :: Test
sanityCheck = testProperty "Sanity Check" (\ a -> (*2) a == ((*2) a :: Int))

-- Run All Module Tests
tests :: [Test]
tests = [ sanityCheck
        , Caesar.test
        ]
