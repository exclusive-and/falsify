module Demo.TestShrinking (tests) where

import Data.Default
import Test.Tasty
import Test.Tasty.Falsify

import qualified Test.Falsify.Generator as Gen

tests :: TestTree
tests = testGroup "Demo.TestShrinking" [
      testProperty "prim" $
        testShrinking (>=) $
          gen $ Gen.prim
    , testPropertyWith expectFailure "mod1" $
         testShrinking (>=) $
            gen $ (`mod` 100) <$> Gen.prim

      -- The following will result in a test failure
      --
      -- We should see in the log both the value generated by @Gen.prim@,
      -- as well as the result. Note that the fmap is on the /outside/ now.
    , testProperty "mod2" $
        testShrinking (>=) $
          (`mod` 100) <$> gen Gen.prim
    ]
  where
    expectFailure :: TestOptions
    expectFailure = def { expectFailure = ExpectFailure }

