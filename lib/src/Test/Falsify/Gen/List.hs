module Test.Falsify.Gen.List (
    list
  , elem
  , pick
  , pickBiased
    -- ** Shuffling
  , shuffle
  , permutation
  ) where

import Prelude hiding (elem)

import Control.Monad
import Data.Falsify.List (Permutation)
import Data.Falsify.List qualified as List
import Data.Falsify.Marked
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Test.Falsify.Gen
import Test.Falsify.Gen.Shrinking
import Test.Falsify.Gen.Simple
import Test.Falsify.Range (Range)
import Test.Falsify.Range qualified as Range

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

-- | Generate list of specified length
--
-- Shrinking behaviour:
--
-- * The length of the list will shrink as specified by the given range.
-- * We can drop random elements from the list, but prefer to drop them
--   from near the /end/ of the list.
--
-- Note on shrinking predictability: in the case that the specified 'Range' has
-- an origin which is neither the lower bound nor the upper bound (and only in
-- that case), 'list' can have confusing shrinking behaviour. For example,
-- suppose we have a range @(0, 10)@ with origin 5. Then we could start by
-- generating an intermediate list of length of 10 and then subsequently drop 5
-- elements from that, resulting in an optimal list length. However, we can now
-- shrink that length from 10 to 2 (which is closer to 5, after all), but now we
-- only have 2 elements to work with, and hence the generated list will now drop
-- from 5 elements to 2. This is not necessarily a problem, because that length
-- 2 can now subsequently shrink further towards closer to the origin (5), but
-- nonetheless it might result in confusing intermediate shrinking steps.
list :: Range Word -> Gen a -> Gen [a]
list len gen = do
    -- We do /NOT/ mark this call to 'inRange' as 'withoutShrinking': it could
    -- shrink towards larger values, in which case we really need to generate
    -- more elements. This doesn't really have any downsides: it merely means
    -- that we would prefer to shrink towards a prefix of the list first, before
    -- we try to drop random other elements from the list.
    --
    -- If we have an expression such as @(,) <$> list .. <*> list@, the two
    -- lists will be shrunk independently from each other due to the branching
    -- point above them. Hence, it doesn't matter if first generator uses "fewer
    -- samples" as it shrinks.
    n <- inRange len

    -- Generate @n@ marks, indicating for each element if we want to keep that
    -- element or not, so that we can drop elements from the middle of the list.
    --
    -- Due to the left-biased nature of shrinking, this will shrink towards
    -- dropped elements (@False@ values) near the start, but we want them near
    -- the /end/, so we reverse the list.
    marks <- fmap (List.keepAtLeast (Range.origin len) . reverse) $
               replicateM (fromIntegral n) $ mark gen

    -- Finally, generate the elements we want to keep
    catMaybes <$> selectAllKept marks

-- | Choose random element
--
-- Shrinks towards earlier elements.
--
-- NOTE: Does not work on infinite lists (it computes the length of the list).
elem :: NonEmpty a -> Gen a
elem = fmap (\(_before, x, _after) -> x) . pick

-- | Generalization of 'elem' that additionally returns the parts of the list
-- before and after the element
pick :: NonEmpty a -> Gen ([a], a, [a])
pick = \xs ->
    aux [] (NE.toList xs) <$>
      inRange (Range.between (0, length xs - 1))
  where
    aux :: [a] -> [a] -> Int -> ([a], a, [a])
    aux _    []     _ = error "pick: impossible"
    aux prev (x:xs) 0 = (reverse prev, x, xs)
    aux prev (x:xs) i = aux (x:prev) xs (i - 1)

-- | Choose random element from a list
--
-- This is different from 'elem': it avoids first computing the length of the
-- list, and is biased towards elements earlier in the list. The advantage is
-- that this works for infinite lists, too.
--
-- Also returns the elements from the list before and after the chosen element.
pickBiased :: NonEmpty a -> Gen ([a], a, [a])
pickBiased = \xs -> pickChunk [] (List.chunksOfNonEmpty chunkSize xs)
  where
    chunkSize :: Word
    chunkSize = 1_000

    -- We want to avoid computing the length of the list, but equally we don't
    -- want to skew /too/ heavily towards the start of the list. Therefore we
    -- chunk the list (this is lazy), then flip a coin for each chunk, and once
    -- we find a chunk, do an unbiased choice within that chunk.
    pickChunk :: [NonEmpty a] -> NonEmpty (NonEmpty a) -> Gen ([a], a, [a])
    pickChunk prev (chunk :| []) = do
        -- No choice left: we must generate use this chunk
        withChunk prev chunk []
    pickChunk prev (chunk :| next@(n:ns)) = do
        useChunk <- bool True
        if useChunk
          then withChunk prev chunk next
          else pickChunk (chunk:prev) (n :| ns)

    withChunk :: [NonEmpty a] -> NonEmpty a -> [NonEmpty a] -> Gen ([a], a, [a])
    withChunk prev chunk next = do
        (chunkBefore, chunkElem, chunkAfter) <- pick chunk
        return (
            concat $ reverse $ chunkBefore : map NE.toList prev
          , chunkElem
          , chunkAfter ++ concatMap NE.toList next
          )

{-------------------------------------------------------------------------------
  Shuffling
-------------------------------------------------------------------------------}

-- | Shuffle list (construct a permutation)
--
-- Shrinking behaviour: 'shuffle' is defined in terms of 'permutation', which
-- provides some guarantees: it shrinks towards making changes near the /start/
-- of the list, and towards swapping /fewer/ elements of the list.
--
-- It is difficult to define precisely how this affects the resulting list, but
-- we /can/ say that if for a particular counter-example it suffices if two
-- lists are different in /one/ element, then the shuffled list will in fact
-- only be different in /one/ place from the original, and that one element will
-- have been swapped with an immediate neighbour.
shuffle :: [a] -> Gen [a]
shuffle xs =
    flip List.applyPermutation xs <$>
      permutation (fromIntegral $ length xs)

-- | Generate permutation for a list of length @n@
--
-- This is essentially an implemention of Fisher-Yates, in that we generate a
-- series of swaps (i, j), with 1 <= i <= n - 1 and @0 <= j <= i@, except that
--
-- * We can shrink a choice of @i@ (towards 1).
-- * We can drop arbitrary swaps.
--
-- This ensures that we shrink towards making swaps nearer the /start/ of the
-- list, as well as towards /fewer/ swaps.
--
-- We make no attempt to make the permutation canonical; doing so makes it
-- extremely difficult to get predicable shrinking behaviour.
permutation :: Word -> Gen Permutation
permutation 0 = return []
permutation 1 = return []
permutation n = do
    swaps <- mapM (mark . genSwap) [n - 1, n - 2 .. 1]
    catMaybes <$> selectAllKept swaps
  where
    genSwap :: Word -> Gen (Word, Word)
    genSwap i = do
        i' <- inRange $ Range.between (1, i)
        j  <- inRange $ Range.between (i, 0)
        return (i', min i' j)
