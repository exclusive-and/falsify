{-# LANGUAGE CPP #-}

-- | Properties
--
-- Intended for unqualified import.
--
-- Most users will probably use "Test.Tasty.Falsify" instead of this module.
module Test.Falsify.Property (
    Property' (WrapProperty) -- opaque
  , Property
  , runProperty
    -- * Run generators
  , gen
  , genWith
    -- * 'Property' features
  , testFailed
  , assert
  , info
  , discard
  , label
  , collect

  , appendLog
  ) where

import Prelude hiding (log)

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

import Control.Monad.State
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack
import Test.Falsify.Gen (Gen)
import Test.Falsify.Predicate (Predicate)
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Result

-- | Append log from another test run to the current test run
--
-- This is an internal function, used when testing shrinking to include the runs
-- from an unshrunk test and a shrunk test.
appendLog :: Log -> Property' e ()
appendLog (Log log') = mkProperty $ \run@TestRun{runLog = Log log} -> return (
      TestPassed ()
    , run{runLog = Log $ log' ++ log}
    )

{-------------------------------------------------------------------------------
  Definition

  The @Property@ type synonym for properties that use strings are errors is
  defined in "Test.Falsify.Property". We do not define it here, so that we
  cannot by mistake make a function less polymorphic than it should be.
-------------------------------------------------------------------------------}

-- | Property
--
-- A 'Property' is a generator that can fail and keeps a track of some
-- information about the test run.
--
-- In most cases, you will probably want to use 'Test.Falsify.Property.Property'
-- instead, which fixes @e@ at 'String'.
newtype Property' e a = WrapProperty {
      unwrapProperty :: TestResultT e (StateT TestRun Gen) a
    }
  deriving newtype (Functor, Applicative, Monad)

-- | Property that uses strings as errors
type Property = Property' String

-- | Construct property
--
-- This is a low-level function for internal use only.
mkProperty :: (TestRun -> Gen (TestResult e a, TestRun)) -> Property' e a
mkProperty = WrapProperty . TestResultT . StateT

-- | Run property
runProperty :: Property' e a -> Gen (TestResult e a, TestRun)
runProperty = flip runStateT initTestRun . runTestResultT . unwrapProperty

{-------------------------------------------------------------------------------
  'Property' features
-------------------------------------------------------------------------------}

-- | Test failure
testFailed :: e -> Property' e a
testFailed err = mkProperty $ \run -> return (TestFailed err, run)

-- | Discard this test
discard :: Property' e a
discard = mkProperty $ \run -> return (TestDiscarded, run)

-- | Log some additional information about the test
--
-- This will be shown in verbose mode.
info :: String -> Property' e ()
info msg =
    mkProperty $ \run@TestRun{runLog = Log log} -> return (
        TestPassed ()
      , run{runLog = Log $ Info msg : log}
      )

-- | Fail the test if the predicate does not hold
assert :: Predicate '[] -> Property' String ()
assert p =
    case P.eval p of
      Left err -> testFailed err
      Right () -> return ()

-- | Variation on 'collect' that does not rely on 'Show'
--
-- See 'collect' for detailed discussion.
label :: String -> [String] -> Property' e ()
label lbl vals =
    mkProperty $ \run@TestRun{runLabels} -> return (
        TestPassed ()
      , run{runLabels = Map.alter addValues lbl runLabels}
      )
  where
    addValues :: Maybe (Set String) -> Maybe (Set String)
    addValues = Just . Set.union (Set.fromList vals) . fromMaybe Set.empty

-- | Label this test
--
-- See also 'label', which does not rely on 'Show'.
--
-- === Motivation
--
-- Labelling is instrumental in understanding the distribution of test data. For
-- example, consider testing a binary tree type, and we want to test some
-- properties of an @insert@ operation (example from "How to specify it!" by
-- John Hughes):
--
-- > prop_insert_insert :: Property ()
-- > prop_insert_insert = do
-- >   tree     <- gen $ ..
-- >   (k1, v1) <- gen $ ..
-- >   (k2, v2) <- gen $ ..
-- >   assert $ .. (insert k1 v1 $ insert k2 v2 $ tree) ..
--
-- We might want to know in what percentage of tests @k1 == k2@:
--
-- > collect "sameKey" [k1 == k2]
--
-- When we do, @falsify@ will report in which percentage of tests the key
-- are the same, and in which percentage of tests they are not.
--
-- === Labels with multiple values
--
-- In general, a particular label can have multiple values in any given test
-- run. Given a test of @n@ test runs, for each value @v@ reported, @falsify@
-- will report what percentage of the @n@ runs are labelled with @v@. That means
-- that these percentages /may/ not add up to 100%; indeed, if we had
--
-- > collect "sameKey" [True]
-- > ..
-- > collect "sameKey" [False]
--
-- or, equivalently,
--
-- > collect "sameKey" [True, False]
--
-- then /every/ test would have been reported as labelled with @True@ (100%@)
-- /as well as/ with @False@ (also 100%). Of course, if we do (like above)
--
-- > collect "sameKey" [k1 == k2]
--
-- each test will be labelled with /either/ @True@ /or/ @False@, and the
-- percentages /will/ add up to 100%.
--
-- === Difference from QuickCheck
--
-- Since you can call @collect@ anywhere in a property, it is natural that the
-- same label can have /multiple/ values in any given test run. In this regard,
-- @collect@ is closer to QuickCheck's @tabulate@. However, the statistics of
-- @tabulate@ can be difficult to interpret; QuickCheck reports the frequency of
-- a value as a percentage of the /total number of values collected/; the
-- frequency reported by @falsify@ here is always in terms of number of test
-- runs, like @collect@ does in QuickCheck. We therefore opted to use the name
-- @collect@ rather than @tabulate@.
collect :: Show a => String -> [a] -> Property' e ()
collect l = label l . map show

instance MonadFail (Property' String) where
  fail = testFailed

{-------------------------------------------------------------------------------
  Running generators
-------------------------------------------------------------------------------}

-- | Internal auxiliary
genWithCallStack :: forall e a.
     CallStack           -- ^ Explicit argument to avoid irrelevant entries
                         -- (users don't care that 'gen' uses 'genWith').
  -> (a -> Maybe String) -- ^ Entry to add to the log (if any)
  -> Gen a -> Property' e a
genWithCallStack stack f g = mkProperty $ \run -> aux run <$> g
  where
    aux :: TestRun -> a -> (TestResult e a, TestRun)
    aux run@TestRun{runLog = Log log} x = (
          TestPassed x
        , run{ runLog = Log $ case f x of
                 Just entry -> Generated stack entry : log
                 Nothing    -> log
             , runDeterministic = False
             }
        )

-- | Generate value and add it to the log
gen :: (HasCallStack, Show a) => Gen a -> Property' e a
gen = genWithCallStack callStack (Just . show)

-- | Generalization of 'gen' that doesn't depend on a 'Show' instance
--
-- No log entry is added if 'Nothing'.
genWith :: HasCallStack => (a -> Maybe String) -> Gen a -> Property' e a
genWith = genWithCallStack callStack
