-- | Generator
--
-- Intended for qualified import.
--
-- > import Test.Falsify.Generator (Gen)
-- > import qualified Test.Falsify.Generator qualified as Gen
module Test.Falsify.Generator (
    -- * Definition
    Gen -- opaque
    -- ** Specific generators
    -- * Simple (non-compound)
  , bool
  , integral
    -- ** Compound
  , list
    -- ** Primitive
  , prim
  , primWith
    -- * Combinators
  , withoutShrinking
    -- * Execution
  , run
  , shrink
  ) where

import Test.Falsify.Internal.Generator
import Test.Falsify.Reexported.Generator.Compound
import Test.Falsify.Reexported.Generator.Simple