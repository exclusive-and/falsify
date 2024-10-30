module Test.Falsify.Prelude (
    -- * Properties
    Property'
  , Property
  , gen
  , genWith
  , testFailed
  , assert
  , info
  , discard
  , label
  , collect
    -- * Ranges
  , Range
  , between
  , enum
  , withOrigin
  , skewedBy
  , origin
  , ProperFraction(..)
  , Precision(..)
  , constant
  , fromProperFraction
  , towards
  , eval
    -- * Generators
  , Gen
  , bool
  , inRange
  , int
  , choose
  , oneof
  , list
  , elem
  , pick
  , pickBiased
  , shuffle
  , Permutation
  , applyPermutation
  , permutation
  , frequency
  , Tree(Leaf, Branch)
  , drawTree
  , tree
  , bst
  , ShrinkTree
  , fromShrinkTree
  , toShrinkTree
  , IsValidShrink(..)
  , path
  , pathAny
  , Marked(..)
  , Mark(..)
  , selectAllKept
  , mark
  , Fun
  , applyFun
  , pattern Fn
  , pattern Fn2
  , pattern Fn3
  , fun
  , Function(..)
  , (:->)
  , functionMap
  , WordN(..)
  , wordN
  , properFraction
  , withoutShrinking
  , shrinkToOneOf
  , firstThen
  , shrinkWith
  , shrinkToNothing
  , bindIntegral
  , perturb
  , prim
  , primWith
  , exhaustive
  , captureLocalTree
  , bindWithoutShortcut
  ) where

import Prelude hiding (either, elem, properFraction)

import Data.Falsify.List
import Data.Falsify.Marked
import Data.Falsify.Tree
import Test.Falsify.Gen
import Test.Falsify.Gen.Choice
import Test.Falsify.Gen.Distribution
import Test.Falsify.Gen.Function
import Test.Falsify.Gen.List
import Test.Falsify.Gen.Precision
import Test.Falsify.Gen.ShrinkTree
import Test.Falsify.Gen.Simple
import Test.Falsify.Gen.Tree
import Test.Falsify.Property
import Test.Falsify.Range
import Test.Falsify.Shrinking
