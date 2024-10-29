module Test.Falsify.Gen.ShrinkTree (
    ShrinkTree
  , fromShrinkTree
  , toShrinkTree
  , path
  , pathAny
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Tree qualified as Rose
import Data.Void
import Test.Falsify.Gen
import Test.Falsify.Gen.Choice
import Test.Falsify.Gen.List
import Test.Falsify.Shrinking

type ShrinkTree = Rose.Tree

-- | Generate semi-random path through the tree
--
-- Will only construct paths that satisfy the given predicate (typically, a
-- property that is being tested).
--
-- Shrinks towards shorter paths, and towards paths that use subtrees that
-- appear earlier in the list of subtrees at any node in the tree.
--
-- See also 'pathAny'.
path :: forall a p n.
     (a -> IsValidShrink p n) -- ^ Predicate
  -> ShrinkTree a
  -> Gen (Either n (NonEmpty p))
path validShrink = \(Rose.Node a as) ->
    case validShrink a of
      InvalidShrink n -> pure $ Left n
      ValidShrink   p -> Right <$> go p as
  where
    -- We only want to pick a shrunk value that matches the predicate, but we
    -- potentially waste a /lot/ of work if we first evaluate the predicate for
    -- /all/ potential shrunk values and then choose. So, instead we choose
    -- first, evaluate the predicate, and if it fails, choose again.
    go :: p -> [Rose.Tree a] -> Gen (NonEmpty p)
    go p []     = pure (p :| [])
    go p (a:as) = do
        (before, a', after) <- pickBiased (a :| as)

        case checkPred a' of
          Nothing ->
            -- Not a valid shrink step. Pick a different one.
            go p (before ++ after)
          Just (p', as') ->
            -- Found a valid shrink step.
            --
            -- We only call @choose@ once we found a valid shrink step,
            -- otherwise we would skew very heavily towards shorter paths.
            choose
              (pure (p :| []))
              (NE.cons p <$> go p' as')

    checkPred :: Rose.Tree a -> Maybe (p, [Rose.Tree a])
    checkPred (Rose.Node a as) =
       case validShrink a of
         InvalidShrink _ -> Nothing
         ValidShrink   b -> Just (b, as)

-- | Variation on 'path' without a predicate.
pathAny :: ShrinkTree a -> Gen (NonEmpty a)
pathAny = fmap (either absurd id) . path ValidShrink
