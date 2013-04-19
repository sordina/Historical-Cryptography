-- Framework
import Test.Framework                       (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Modules
import qualified Codec.Encryption.Historical.Caesar.Test as Caesar

main :: IO ()
main = defaultMain tests

ignore :: Functor f => f a -> f ()
ignore = fmap (const ())

sanityCheck :: Test
sanityCheck = testProperty "Sanity Check" (\ a -> (*2) a == ((*2) a :: Int))

tests :: [Test]
tests = [ sanityCheck
        , Caesar.test
        ]

-- Notes:

-- import Test.Framework.Providers.HUnit
-- import Prelude hiding ((||),(&&))
-- import Test.QuickCheck hiding ((==>))
-- import Test.HUnit hiding (Test)

-- testGroup "cases"      $ zipWith testCase     numbering $ [] ,
