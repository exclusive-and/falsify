{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | We test the 'GenDefault' machinery by defining a tag, deriving some 'GenDefault'
-- instances, and asserting that the derived generators yield more than one distinct
-- value.
module TestSuite.GenDefault (tests) where

import Control.Monad (replicateM)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import GHC.Exts (IsList, IsString)
import GHC.Generics (Generic)
import Test.Falsify.Gen
import Test.Falsify.Gen.Default
import Test.Falsify.Gen.Std
import Test.Falsify.Interactive
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

data Tag

-- Exercise ViaTag

deriving via (ViaTag Std Int) instance GenDefault Tag Int
deriving via (ViaTag Std Char) instance GenDefault Tag Char

-- Exercise ViaList

newtype AList a = AList [a]
  deriving newtype (Eq, Ord, Show, IsList)

deriving via (ViaList (AList a) 0 2)
    instance GenDefault Tag a => GenDefault Tag (AList a)

-- Exercise ViaString

newtype AString = AString String
  deriving newtype (Eq, Ord, Show, IsString)
  deriving (GenDefault Tag) via (ViaString AString 0 2)

-- Exercise ViaEnum

data Choice = ChoiceA | ChoiceB
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (GenDefault Tag) via (ViaEnum Choice)

-- Exercise ViaGeneric

deriving via (ViaGeneric Tag (Maybe a))
    instance GenDefault Tag a => GenDefault Tag (Maybe a)

data Record = Record !Int !(Maybe Record)
  deriving stock (Eq, Ord, Show, Generic)
  deriving (GenDefault Tag) via (ViaGeneric Tag Record)

data GenCase where
  GenCase :: Ord a => String -> Gen a -> GenCase

genDefaultByProxy :: GenDefault Tag a => Proxy a -> Gen a
genDefaultByProxy _ = genDefault (Proxy @Tag)

mkGenCase :: (Ord a, GenDefault Tag a) => String -> Proxy a -> GenCase
mkGenCase name = GenCase name . genDefaultByProxy

genCases :: [GenCase]
genCases =
  [ mkGenCase "Int" (Proxy @Int)
  , mkGenCase "Char" (Proxy @Char)
  , mkGenCase "Choice" (Proxy @Choice)
  , mkGenCase "AList" (Proxy @(AList Char))
  , mkGenCase "AString" (Proxy @AString)
  , mkGenCase "Record" (Proxy @Record)
  ]

testGenCase :: GenCase -> TestTree
testGenCase (GenCase name gen) = testCase name $ do
  xs <- fmap Set.fromList (replicateM 10 (sample gen))
  assertBool "generates more than one value" (Set.size xs > 1)

tests :: TestTree
tests = testGroup "TestSuite.GenDefault" (fmap testGenCase genCases)
