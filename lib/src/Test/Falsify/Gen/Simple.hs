-- | Simple (i.e., non-compound) generators
module Test.Falsify.Gen.Simple (
    bool
  , inRange
  , int
  ) where

import Prelude hiding (properFraction)

import Data.Bits
import Data.Word

import Test.Falsify.Gen.Monad
import Test.Falsify.SampleTree (Sample(..), sampleValue)
import Test.Falsify.Gen.Precision

import Test.Falsify.Range as Range

{-------------------------------------------------------------------------------
  Simple generators
-------------------------------------------------------------------------------}

-- | Generate random bool, shrink towards the given value
--
-- Chooses with equal probability between 'True' and 'False'.
bool :: Bool -> Gen Bool
bool target = aux . sampleValue <$> primWith shrinker
  where
    aux :: Word64 -> Bool
    aux x | msbSet x  = not target
          | otherwise = target

    msbSet :: forall a. FiniteBits a => a -> Bool
    msbSet x = testBit x (finiteBitSize (undefined :: a) - 1)

    shrinker :: Sample -> [Word64]
    shrinker (Shrunk 0) = []
    shrinker _          = [0]

{-------------------------------------------------------------------------------
  Integral ranges
-------------------------------------------------------------------------------}

-- | Generate value in the specified range
inRange :: Range a -> Gen a
inRange r = Range.eval properFraction r

-- | Type-specialization of 'inRange'
int :: Range Int -> Gen Int
int = inRange

