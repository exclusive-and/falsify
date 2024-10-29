module Test.Falsify.Gen.Tree (
    tree
  , bst
  ) where

import Control.Monad
import Data.Falsify.Marked
import Data.Falsify.Tree (Tree(..), Interval(..), Endpoint(..))
import Data.Falsify.Tree qualified as Tree
import Test.Falsify.Gen
import Test.Falsify.Gen.Simple
import Test.Falsify.Range (Range)
import Test.Falsify.Range qualified as Range
import Test.Falsify.Shrinking

{-------------------------------------------------------------------------------
  Binary trees
-------------------------------------------------------------------------------}

-- | Generate binary tree
tree :: forall a. Range Word -> Gen a -> Gen (Tree a)
tree size gen = do
    n <- inRange size
    t <- Tree.keepAtLeast (Range.origin size) . Tree.propagate <$> go n
    Tree.genKept t
  where
    go :: Word -> Gen (Tree (Marked Gen a))
    go 0 = return Leaf
    go n = do
        -- Generate element at the root
        x <- mark gen

        -- Choose how many elements to put in the left subtree
        --
        -- This ranges from none (right-biased) to all (left-biased), shrinking
        -- towards half the number of elements: hence, towards a balanced tree.
        inLeft <- inRange $ Range.withOrigin (0, n - 1) ((n - 1) `div` 2)
        let inRight = (n - 1) - inLeft
        Branch x <$> go inLeft <*> go inRight

-- | Construct binary search tree
--
-- Shrinks by replacing entire subtrees by the empty tree.
bst :: forall a b. Integral a => (a -> Gen b) -> Interval a -> Gen (Tree (a, b))
bst gen = go >=> traverse (\a -> (a,) <$> gen a)
  where
    go :: Interval a -> Gen (Tree a)
    go i =
        case Tree.inclusiveBounds i of
          Nothing       -> pure Leaf
          Just (lo, hi) -> firstThen id (const Leaf) <*> go' lo hi

    -- inclusive bounds, lo <= hi
    go' :: a -> a -> Gen (Tree a)
    go' lo hi = Branch mid
            <$> go (Interval (Inclusive lo) (Exclusive mid))
            <*> go (Interval (Exclusive mid) (Inclusive hi))
      where
        mid :: a
        mid = lo + ((hi - lo) `div` 2)
