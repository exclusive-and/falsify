-- | Support for @falsify@ in the @tasty@ framework
--
-- As is customary, this also re-exports parts of the @falsify@ API, but not
-- modules such as "Test.Falsify.Range" that are intended to be imported
-- qualified.
module Test.Tasty.Falsify (
    -- * Test property
    testProperty
    -- * Configure test behaviour
  , TestOptions(..)
  , Verbose(..)
  , ExpectFailure(..)
  , testPropertyWith
    -- * Re-exports
  , module Test.Falsify.Property
    -- ** Generators
  , Gen
    -- ** Functions
  , pattern Gen.Fn
  , pattern Gen.Fn2
  , pattern Gen.Fn3
  ) where

import Test.Falsify.Gen (Gen)
import Test.Falsify.Tasty
import Test.Falsify.Property

import qualified Test.Falsify.Gen.Function as Gen
