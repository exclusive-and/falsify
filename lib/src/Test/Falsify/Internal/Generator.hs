-- | Export the public API of the generator, hiding implementation details.
--
-- This is the only module that should import from
-- @Test.Falsify.Internal.Generator.*@.
--
-- Intended for unqualified import.
module Test.Falsify.Internal.Generator (
    Gen -- opaque
  , bindWithoutShortcut
    -- * Execution
  , runGen
  , shrinkFrom
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

import Test.Falsify.Gen.Monad
import Test.Falsify.Shrinking
