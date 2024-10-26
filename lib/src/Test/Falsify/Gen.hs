module Test.Falsify.Gen (
    -- * Definition
    Gen
  , runGen
  , bindWithoutShortcut
    -- * Primitive generators
  , prim
  , primWith
  , exhaustive
  , captureLocalTree
    -- * Generator independence
  , bindIntegral
  , perturb
    -- * Combinators
  , withoutShrinking
  ) where

import Control.Monad
import Control.Selective
import Data.Falsify.Integer (Bit(..), encIntegerEliasG)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Word
import Optics.Core hiding (lens)
import Test.Falsify.SampleTree hiding (map)
import Test.Falsify.Search

-- | Generator of a random value
--
-- Generators support \"internal integrated shrinking\". Shrinking is /integrated/
-- in the sense of Hedgehog, meaning that we don't write a separate shrinker at all,
-- but the shrink behaviour is implied by the generator. For example, if you have a generator
-- @genList@ for a list of numbers, then
--
-- > filter even <$> genList
--
-- will only generate even numbers, and that property is automatically preserved
-- during shrinking. Shrinking is /internal/ in the sense of Hypothesis, meaning that
-- unlike in Hedgehog, shrinking works correctly even in the context of monadic bind.
-- For example, if you do
--
-- > do n <- genListLength
-- >    replicateM n someOtherGen
--
-- then we can shrink @n@ and the results from @someOtherGen@ in any order
-- (that said, users may prefer to use the dedicated
-- 'Test.Falsify.Gen.Compound.list' generator for this purpose, which improves on this in
-- a few ways).
newtype Gen a = Gen { runGen :: SampleTree -> (a, [SampleTree]) }
  deriving stock (Functor)

instance Applicative Gen where
  pure x = Gen $ \_st -> (x, [])
  (<*>)  = ap

instance Monad Gen where
  return  = pure
  x >>= f = Gen $ \(Inf s l r) ->
      let (a, ls) = runGen x l
          (b, rs) = runGen (f a) r
      in (b, combineShrunk s (l :| ls) (r :| rs))

instance Selective Gen where
  select e f = Gen $ \(Inf s l r) -> do
      let (ma, ls) = runGen e l
      case ma of
        Left a ->
          let (f', rs) = runGen f r
          in (f' a, combineShrunk s (l :| ls) (r :| rs))
        Right b ->
          (b, combineShrunk s (l :| ls) (r :| []))

-- NOTE: 'Gen' is /NOT/ an instance of 'Alternative'; this would not be
-- compatible with the generation of infinite data structures. For the same
-- reason, we do not have a monad transformer version of Gen either.

{-------------------------------------------------------------------------------
  Primitive generators
-------------------------------------------------------------------------------}

-- | Uniform selection of 'Word64', shrinking towards 0, using binary search
--
-- This is a primitive generator; most users will probably not want to use this
-- generator directly.
prim :: Gen Word64
prim = sampleValue <$> primWith (binarySearch . sampleValue)

-- | Generalization of 'prim' that allows to override the shrink behaviour
--
-- This is only required in rare circumstances. Most users will probably never
-- need to use this generator.
primWith :: (Sample -> [Word64]) -> Gen Sample
primWith f = Gen $ \(Inf s l r) -> (
      s
    , (\s' -> SampleTree (Shrunk s') l r) <$> f s
    )

-- | Generate arbitrary value @x <= n@
--
-- Unlike 'prim', 'exhaustive' does not execute binary search. Instead, /all/
-- smaller values are considered. This is potentially very expensive; the
-- primary use case for this generator is testing shrinking behaviour, where
-- binary search can lead to some unpredicatable results.
--
-- This does /NOT/ do uniform selection: for small @n@, the generator will with
-- overwhelming probability produce @n@ itself as initial value.
--
-- This is a primitive generator; most users will probably not want to use this
-- generator directly.
exhaustive :: Word64 -> Gen Word64
exhaustive n =
    min n . sampleValue <$> primWith (completeSearch . sampleValue)
  where
    completeSearch :: Word64 -> [Word64]
    completeSearch 0 = []
    completeSearch x = takeWhile (<= n) [0 .. pred x]

-- | Capture the local sample tree
--
-- This generator does not shrink.
captureLocalTree :: Gen SampleTree
captureLocalTree = Gen $ \st -> (st, [])

-- | Varation on @(>>=)@ that doesn't apply the shortcut to 'Minimal'
--
-- This function is primarily useful for debugging @falsify@ itself; users
-- will probably never need it.
bindWithoutShortcut :: Gen a -> (a -> Gen b) -> Gen b
bindWithoutShortcut x f = Gen $ \(Inf s l r) ->
    let (a, ls) = runGen x l
        (b, rs) = runGen (f a) r
    in (b, combine s (l :| ls) (r :| rs))
  where
    -- Variation on 'combineShrunk' that doesn't apply the shortcut
    combine ::
         Sample
      -> NonEmpty SampleTree -- ^ Original and shrunk left  trees
      -> NonEmpty SampleTree -- ^ Original and shrunk right trees
      -> [SampleTree]
    combine s (l :| ls) (r :| rs) = concat [
          [SampleTree s l' r  | l' <- ls]
        , [SampleTree s l  r' | r' <- rs]
        ]

{-------------------------------------------------------------------------------
  Generator independence
-------------------------------------------------------------------------------}

-- | Selective bind
--
-- Unlike monadic bind, the RHS is generated and shrunk completely independently
-- for each different value of @a@ produced by the LHS.
--
-- This is a generalization of 'bindS' to arbitrary integral values; it is also
-- much more efficient than 'bindS'.
--
-- NOTE: This is only one way to make a generator independent. See 'perturb'
-- for more primitive combinator.
bindIntegral :: Integral a => Gen a -> (a -> Gen b) -> Gen b
bindIntegral x f = x >>= \a -> perturb a (f a)

-- | Run generator on different part of the sample tree depending on @a@
perturb :: Integral a => a -> Gen b -> Gen b
perturb a g = Gen $ \st ->
    let (b, shrunk) = runGen g (view lens st)
    in (b, map (\st' -> set lens st' st) shrunk)
  where
    lens :: Lens' SampleTree SampleTree
    lens = computeLens (encIntegerEliasG $ fromIntegral a)

    computeLens :: [Bit] -> Lens' SampleTree SampleTree
    computeLens []       = castOptic simple
    computeLens (O : bs) = left  % computeLens bs
    computeLens (I : bs) = right % computeLens bs

{-------------------------------------------------------------------------------
  Shrinking combinators
-------------------------------------------------------------------------------}

-- | Disable shrinking in the given generator
--
-- Due to the nature of internal shrinking, it is always possible that a
-- generator gets reapplied to samples that were shrunk wrt to a /different/
-- generator. In this sense, 'withoutShrinking' should be considered to be a
-- hint only.
--
-- This function is only occassionally necessary; most users will probably not
-- need to use it.
withoutShrinking :: Gen a -> Gen a
withoutShrinking (Gen g) = Gen $ aux . g
  where
    aux :: (a, [SampleTree]) -> (a, [SampleTree])
    aux (outcome, _) = (outcome, [])
