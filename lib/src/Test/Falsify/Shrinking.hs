module Test.Falsify.Shrinking (
    shrink
  , Outcome(..)
  , Explanation(..)
  , History(..)
  , shrinkOutcome
  , shrinkHistory
  , shortenTo
  , IsValidShrink(..)
    -- * Shrink trees
  , fromShrinkTree
  , toShrinkTree
    -- * Specialized shrinking strategies
  , shrinkToOneOf
  , firstThen
  , shrinkToNothing
  , mark
  , shrinkWith
  ) where

import Data.Bifunctor
import Data.Falsify.Marked
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Tree qualified as Rose
import Data.Word
import Test.Falsify.Gen
import Test.Falsify.SampleTree (SampleTree(..), Sample(..))

-- | Calculate simple example values that exhibit the given property.
shrink :: forall a p n.
       (a -> IsValidShrink p n)
    -> Gen a
    -> (p, [SampleTree])
    -> Explanation p n
shrink prop gen = \(p, shrunk) -> Explanation p $ go shrunk
  where
    go :: [SampleTree] -> History p n
    go shrunk = greedy [] $ map (runGen gen) shrunk

    greedy :: [n] -> [(a, [SampleTree])] -> History p n
    -- The algorithm is greedy. It always uses the very first value it finds that
    -- exhibits the given property.
    greedy ns [] = DoneShrinking ns
    greedy ns ((a, shrunk):xs) =
        case prop a of
          InvalidShrink n -> greedy (n:ns) xs
          ValidShrink p   -> ShrunkTo p $ go shrunk

data Outcome p n =
    -- | Shrinking found a minimal value that exhibits some property.
    TotalShrink p [n]

    -- | Shrinking stopped on a value, but it was not necessarily minimal.
  | PartialShrink p
  deriving (Show)

-- | Shrink explanation
--
--    * @p@ is the type of \"positive\" values that satisfied the predicate
--      (i.e., valid shrinks).
--
--    * @n@ is the type of \"negative\" values which didn't.
data Explanation p n = Explanation {
      -- | The value we started, before shrinking
      initial :: p

      -- | The full shrink history
    , history :: History p n
    }
  deriving (Show)

-- | 
data History p n =
    -- | We finished shrinking: any further shrinks don't give the same outcome.
    --
    -- The rejected shrinks are recorded as negative values. It can be informative to know
    -- exactly which shrinks cause code act differently.
    DoneShrinking [n]

    -- | We stopped shrinking early.
  | StoppedShrinking

    -- | We found a shrink to a positive value that preserves the outcome of the test.
  | ShrunkTo p (History p n)
  deriving (Show)

shrinkOutcome :: Explanation p n -> Outcome p n
shrinkOutcome (Explanation unshrunk shrunk) = go unshrunk shrunk
  where
    go p = \case
        DoneShrinking ns -> TotalShrink p ns
        StoppedShrinking -> PartialShrink p
        ShrunkTo p' h    -> go p' h

-- | Simplify the shrink explanation to keep only the shrink history
shrinkHistory :: Explanation p n -> NonEmpty p
shrinkHistory (Explanation unshrunk shrunk) = unshrunk :| go shrunk
  where
    go = \case
        DoneShrinking _  -> []
        StoppedShrinking -> []
        ShrunkTo x xs    -> x : go xs

shortenTo :: Maybe Word -> Explanation p n -> Explanation p n
shortenTo Nothing      = id
shortenTo (Just limit) = \case
    explan@Explanation{history} -> explan{history = go limit history}
  where
    go _ (DoneShrinking rej) = DoneShrinking rej
    go _ StoppedShrinking    = StoppedShrinking
    go 0 (ShrunkTo _ _)      = StoppedShrinking
    go n (ShrunkTo x xs)     = ShrunkTo x (go (n - 1) xs)

-- | Does a given shrunk value represent a valid shrink step?
data IsValidShrink p n =
    ValidShrink p
  | InvalidShrink n
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Mapping
-------------------------------------------------------------------------------}

instance Functor (Explanation p) where
  fmap = second

instance Functor (History p) where
  fmap = second

instance Bifunctor Explanation where
  bimap f g (Explanation x xs) = Explanation (f x) (bimap f g xs)

instance Bifunctor History where
  bimap f g = \case
      DoneShrinking ns -> DoneShrinking (map g ns)
      StoppedShrinking -> StoppedShrinking
      ShrunkTo x xs    -> ShrunkTo (f x) (bimap f g xs)

{-------------------------------------------------------------------------------
  Shrink trees
-------------------------------------------------------------------------------}

-- | Construct generator from shrink tree
--
-- This provides compatibility with Hedgehog-style integrated shrinking.
--
-- This is O(n^2) in the number of shrink steps: as this shrinks, the generator
-- is growing a path of indices which locates a particular value in the shrink
-- tree (resulting from unfolding the provided shrinking function). At each
-- step during the shrinking process the shrink tree is re-evaluated and the
-- next value in the tree is located; since this path throws linearly, the
-- overall cost is O(n^2).
--
-- The O(n^2) cost is only incurred on /locating/ the next element to be tested;
-- the property is not reevaluated at already-shrunk values.
fromShrinkTree :: Rose.Tree a -> Gen a
fromShrinkTree = go
  where
    go :: Rose.Tree a -> Gen a
    go (Rose.Node x xs) = do
        next <- Nothing `shrinkToOneOf` map Just xs
        case next of
          Nothing -> return x
          Just x' -> go x'

-- | Expose the full shrink tree of a generator
--
-- This generator does not shrink.
toShrinkTree :: forall a. Gen a -> Gen (Rose.Tree a)
toShrinkTree gen =
    Rose.unfoldTree aux . runGen gen <$> captureLocalTree
  where
    aux :: (a, [SampleTree]) -> (a,[(a, [SampleTree])])
    aux (x, shrunk) = (x, map (runGen gen) shrunk)

{-------------------------------------------------------------------------------
  Specialized shrinking behaviour
-------------------------------------------------------------------------------}

-- | Start with @x@, then shrink to one of the @xs@
--
-- Once shrunk, will not shrink again.
--
-- Minimal value is the first shrunk value, if it exists, and the original
-- otherwise.
shrinkToOneOf :: forall a. a -> [a] -> Gen a
shrinkToOneOf x xs =
    aux <$> primWith shrinker
  where
    aux :: Sample -> a
    aux (NotShrunk _) = x
    aux (Shrunk    i) = index i xs

    -- When we shrink, we will try a bunch of new sample trees; we must ensure
    -- that we can try /any/ of the possible shrunk values.
    --
    -- We use this to implement 'fromShrinkTree'. Here, we explore a rose tree
    -- of possibilities; at every level in the tree, once we make a choice,
    -- we should commit to that choice and not consider it over and over again.
    -- Thus, once shrunk, we should not shrink any further.
    shrinker :: Sample -> [Word64]
    shrinker (Shrunk _)    = []
    shrinker (NotShrunk _) = zipWith const [0..] xs

    -- Index the list of possible shrunk values. This is a bit like @(!!)@ from
    -- the prelude, but with some edge cases.
    --
    -- - If the list is empty, we return the unshrunk value.
    -- - Otherwise, if the index exceeds the bounds, we return the last element.
    --
    -- These two special cases can arise in one of two circumstances:
    --
    -- - When we run the generator against the 'Minimal' tree. This will give us
    --   a @Shrunk 0@ value, independent of what the specified shrinking
    --   function does, and it is important that we produce the right value.
    -- - When the generator is run against a sample tree that was shrunk wrt to
    --   a /different/ generator. In this case the value could be anything;
    --   we return the final ("least preferred") element, and then rely on
    --   later shrinking to replace this with a more preferred element.
    index :: Word64 -> [a] -> a
    index _ []     = x
    index _ [y]    = y
    index 0 (y:_)  = y
    index n (_:ys) = index (n - 1) ys

-- | Generator that always produces @x@ as initial value, and shrinks to @y@
firstThen :: forall a. a -> a -> Gen a
firstThen x y = x `shrinkToOneOf` [y]

-- | Start with @Just x@ for some @x@, then shrink to @Nothing@
shrinkToNothing :: Gen a -> Gen (Maybe a)
shrinkToNothing g = firstThen Just (const Nothing) <*> g

-- | Mark an element, shrinking towards 'Drop'
--
-- This is similar to 'shrinkToNothing', except that 'Marked' still has a value
-- in the 'Drop' case: marks are merely hints, that we may or may not use.
mark :: Gen a -> Gen (Marked Gen a)
mark x = flip Marked x <$> firstThen Keep Drop

-- | Shrink with provided shrinker
--
-- This provides compatibility with QuickCheck-style manual shrinking.
--
-- Defined in terms of 'fromShrinkTree'; see discussion there for some
-- notes on performance.
shrinkWith :: forall a. (a -> [a]) -> Gen a -> Gen a
shrinkWith f gen = do
    -- It is critical that we do not apply normal shrinking of the 'SampleTree'
    -- here (not even to 'Minimal'). If we did, then the resulting shrink tree
    -- would change, and we would be unable to iteratively construct a path
    -- through the shrink tree.
    --
    -- Of course, it can still happen that the generator gets reapplied in a
    -- different context; we must take this case into account in
    -- 'shrinkToOneOf'.
    x <- withoutShrinking gen
    fromShrinkTree $ Rose.unfoldTree (\x' -> (x', f x')) x
