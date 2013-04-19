import Test.Framework (Test)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO () 
main = defaultMain tests

ignore :: Functor f => f a -> f () 
ignore = fmap (const ())

numbering :: [String]
numbering = map (("Test " ++) . (show :: Int -> String)) [1..]

dummyProperties :: [Test]
dummyProperties = zipWith testProperty numbering [ \ a -> (*2) a == ((*2) a :: Int)
                                                 , \ a -> (*2) a == ((*2) $ a :: Int)
                                                 -- , \ a -> (*3) a == ((*2) $ a :: Int) -- Failing dummy test
                                                 ]

tests :: [Test]
tests = [ testGroup "dummyProperties" dummyProperties
        ]

-- Notes:

-- import Test.Framework.Providers.HUnit
-- import Prelude hiding ((||),(&&))
-- import Test.QuickCheck hiding ((==>))
-- import Test.HUnit hiding (Test)

-- testGroup "cases"      $ zipWith testCase     numbering $ [] ,
