module Test.Falsify.Result (
    TestRun (..)
  , initTestRun
  , Log (..)
  , LogEntry (..)
  , TestResult (..)
  , resultIsValidShrink
  , TestResultT (..)
  ) where

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import GHC.Stack
import Test.Falsify.Shrinking

{-------------------------------------------------------------------------------
  Information about a test run
-------------------------------------------------------------------------------}

data TestRun = TestRun {
      runLog :: Log

      -- | Did we generate any values in this test run?
      --
      -- If not, there is no point running the test more than once (with
      -- different seeds), since the test is deterministic.
    , runDeterministic :: Bool

      -- | Labels
    , runLabels :: Map String (Set String)
    }
  deriving (Show)

data LogEntry =
    -- | Generated a value
    --
    -- We record the value that was generated as well as /where/ we generated it
    Generated CallStack String

    -- | Some additional information
  | Info String
  deriving (Show)

-- | Log of the events happened during a test run
--
-- The events are recorded in reverse chronological order
newtype Log = Log [LogEntry]
  deriving (Show)

initTestRun :: TestRun
initTestRun = TestRun {
      runLog           = Log []
    , runDeterministic = True
    , runLabels        = Map.empty
    }

{-------------------------------------------------------------------------------
  Test result
-------------------------------------------------------------------------------}

-- | Test result
data TestResult e a =
    -- | Test was successful
    --
    -- Under normal circumstances @a@ will be @()@.
    TestPassed a

    -- | Test failed
  | TestFailed e

    -- | Test was discarded
    --
    -- This is neither a failure nor a success, but instead is a request to
    -- discard this PRNG seed and try a new one.
  | TestDiscarded
  deriving stock (Show, Functor)

-- | A test result is a valid shrink step if the test still fails
resultIsValidShrink ::
     (TestResult e a, TestRun)
  -> IsValidShrink (e, TestRun) (Maybe a, TestRun)
resultIsValidShrink (result, run) =
    case result of
      TestFailed e  -> ValidShrink   (e       , run)
      TestDiscarded -> InvalidShrink (Nothing , run)
      TestPassed a  -> InvalidShrink (Just a  , run)

{-------------------------------------------------------------------------------
  Monad-transformer version of 'TestResult'
-------------------------------------------------------------------------------}

newtype TestResultT e m a = TestResultT {
      runTestResultT :: m (TestResult e a)
    }
  deriving (Functor)

instance Monad m => Applicative (TestResultT e m) where
  pure x = TestResultT $ pure (TestPassed x)
  (<*>)  = ap

instance Monad m => Monad (TestResultT e m) where
  return  = pure
  x >>= f = TestResultT $ runTestResultT x >>= \case
              TestPassed a  -> runTestResultT (f a)
              TestFailed e  -> pure $ TestFailed e
              TestDiscarded -> pure $ TestDiscarded
