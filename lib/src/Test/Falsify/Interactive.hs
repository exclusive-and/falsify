-- | Utilities for interaction with falsify in ghci
module Test.Falsify.Interactive (
    falsify
  , falsify'
  , sample
  , shrink
  , shrink'
    -- * Falsify prelude
  , module Test.Falsify.Prelude
  ) where

import Data.Bifunctor
import Data.Default
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import System.Random.SplitMix
import Test.Falsify (Failure, Success, TotalDiscarded)
import Test.Falsify qualified as Falsify
import Test.Falsify.Gen (Gen, runGen)
import Test.Falsify.Property
import Test.Falsify.ReplaySeed
import Test.Falsify.SampleTree qualified as SampleTree
import Test.Falsify.Sanity
import Test.Falsify.Shrinking hiding (shrink)

import Test.Falsify.Prelude qualified

-- | Sample generator
sample :: Gen a -> IO a
sample g = do
    prng <- initSMGen
    let (x, _shrunk) = runGen g (SampleTree.fromPRNG prng)
    return x

-- | Shrink counter-example
--
-- This will run the generator repeatedly until it finds a counter-example to
-- the given property, and will then shrink it.
--
-- Returns 'Nothing' if no counter-example could be found.
shrink :: forall a. (a -> Bool) -> Gen a -> IO (Maybe a)
shrink p g = falsify $ testGen' (\x -> aux x $ p x) g
  where
    aux :: a -> Bool -> Either a ()
    aux _ True  = Right ()
    aux x False = Left x

-- | Generalization of 'shrink'. Returns the full shrink history.
shrink' :: forall e a. (a -> Maybe e) -> Gen a -> IO (Maybe (NonEmpty e))
shrink' p g = falsify' $ testGen' (aux . p) g
  where
    aux :: Maybe e -> Either e ()
    aux Nothing  = Right ()
    aux (Just x) = Left x

-- | Try to falsify the given property
--
-- Reports the counter-example, if we find any.
falsify :: forall e a. Property' e a -> IO (Maybe e)
falsify = fmap (fmap NE.last) . falsify'

-- | Generalization of 'falsify' that reports the full shrink history
falsify' :: forall e a. Property' e a -> IO (Maybe (NonEmpty e))
falsify' = fmap aux . Falsify.falsify def
  where
    aux ::
         ( ReplaySeed
         , [Success a]
         , TotalDiscarded
         , Maybe (Failure e)
         )
      -> Maybe (NonEmpty e)
    aux (_seed, _successes, _discarded, failure) =
        case failure of
          Nothing -> Nothing
          Just f  -> Just $ shrinkHistory $ first fst $ Falsify.failureRun f
