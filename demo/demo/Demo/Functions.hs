module Demo.Functions (tests) where

import Data.Default
import Data.Word
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Prelude
import Test.Tasty
import Test.Tasty.Falsify

tests :: TestTree
tests = testGroup "Demo.Functions" [
      testPropertyWith expectFailure "listToBool" prop_listToBool
    ]
  where
    expectFailure :: TestOptions
    expectFailure = def {
          expectFailure   = ExpectFailure
        , overrideVerbose = Just Verbose
        }

prop_listToBool :: Property ()
prop_listToBool = do
    Fn (f :: [Word8] -> Bool) <- gen $ fun (bool False)
    assert $ P.eq .$ ("lhs", f [3, 1, 4, 2])
                  .$ ("rhs", f [1, 6, 1, 8])


