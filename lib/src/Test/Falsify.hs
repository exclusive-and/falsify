-- | Test falsification algorithm
--
-- Intended for qualified import.
--
-- > import Test.Falsify (Success, Failure, falsify)
-- > import qualified Test.Falsify as Falsify
module Test.Falsify (
    -- * Options
    Options(..)
    -- * Results
  , Success(..)
  , Failure(..)
  , TotalDiscarded(..)
    -- * Test algorithm
  , falsify
  ) where

import Prelude hiding (log)

import Data.Bifunctor
import Data.Default
import System.Random.SplitMix
import Test.Falsify.Gen
import Test.Falsify.Property
import Test.Falsify.ReplaySeed
import Test.Falsify.Result
import Test.Falsify.SampleTree (SampleTree)
import Test.Falsify.SampleTree qualified as SampleTree
import Test.Falsify.Shrinking

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Options for running a test
data Options = Options {
      -- | Number of test cases to generate
      tests :: Word

      -- | Number of shrinks allowed before failing a test
    , maxShrinks :: Maybe Word

      -- | Random seed to use for replaying a previous test run
    , replay :: Maybe ReplaySeed

      -- | Maximum number of discarded test per successful test
    , maxRatio :: Word
    }

instance Default Options where
  def = Options {
        tests      = 100
      , maxShrinks = Nothing
      , replay     = Nothing
      , maxRatio   = 100
      }

{-------------------------------------------------------------------------------
  Driver
-------------------------------------------------------------------------------}

data Success a = Success {
      successResult :: a
    , successSeed   :: ReplaySeed
    , successRun    :: TestRun
    }
  deriving (Show)

data Failure e = Failure {
      failureSeed :: ReplaySeed
    , failureRun  :: Explanation (e, TestRun) TestRun
    }
  deriving (Show)

newtype TotalDiscarded = TotalDiscarded Word

-- | Run a test: attempt to falsify the given property
--
-- We return
--
-- * initial replay seed (each test also records its own seed)
-- * successful tests
-- * how many tests we discarded
-- * the failed test (if any).
falsify :: forall e a.
     Options
  -> Property' e a
  -> IO (ReplaySeed, [Success a], TotalDiscarded, Maybe (Failure e))
falsify opts prop = do
    acc <- initDriverState opts
    (successes, discarded, mFailure) <- go acc
    return (
        splitmixReplaySeed (prng acc)
      , successes
      , TotalDiscarded discarded
      , mFailure
      )
  where
    go :: DriverState a -> IO ([Success a], Word, Maybe (Failure e))
    go acc | todo acc == 0 = return (successes acc, discardedTotal acc, Nothing)
    go acc = do
        let now, later :: SMGen
            (now, later) = splitSMGen (prng acc)

            st :: SampleTree
            st = SampleTree.fromPRNG now

            result :: TestResult e a
            run    :: TestRun
            shrunk :: [SampleTree]
            ((result, run), shrunk) = runGen (runProperty prop) st

        case result of
          -- Test passed
          TestPassed x -> do
            let success :: Success a
                success = Success {
                    successResult = x
                  , successSeed   = splitmixReplaySeed now
                  , successRun    = run
                  }
            if runDeterministic run then
              case (successes acc, discardedTotal acc) of
                ([], 0)    -> return ([success], 0, Nothing)
                _otherwise -> error "falsify.go: impossible"
            else
              go $ withSuccess later success acc

          -- Test failed
          --
          -- We ignore the failure message here, because this is the failure
          -- message before shrinking, which we are typically not interested in.
          TestFailed e -> do
            let explanation :: Explanation (e, TestRun) TestRun
                explanation =
                    shortenTo (maxShrinks opts) . second snd $
                      shrink
                        resultIsValidShrink
                        (runProperty prop)
                        ((e, run), shrunk)

                -- We have to be careful here: if the user specifies a seed, we
                -- will first /split/ it to run the test (call to splitSMGen,
                -- above). This means that the seed we should provide for the
                -- test is the seed /before/ splitting.
                failure :: Failure e
                failure = Failure {
                      failureSeed = splitmixReplaySeed (prng acc)
                    , failureRun  = explanation
                    }

            return (successes acc, discardedTotal acc, Just failure)

          -- Test discarded, but reached maximum already
          TestDiscarded | discardedForTest acc == maxRatio opts ->
            return (successes acc, discardedTotal acc, Nothing)

          -- Test discarded; continue.
          TestDiscarded ->
            go $ withDiscard later acc

{-------------------------------------------------------------------------------
  Internal: driver state
-------------------------------------------------------------------------------}

data DriverState a = DriverState {
      -- | State of the PRNG after the previously executed test
      prng :: SMGen

      -- | Accumulated successful tests
    , successes :: [Success a]

      -- | Number of tests still to execute
    , todo :: Word

      -- | Number of tests we discarded so far (for this test)
    , discardedForTest :: Word

      -- | Number of tests we discarded (in total)
    , discardedTotal :: Word
    }
  deriving (Show)

initDriverState :: Options -> IO (DriverState a)
initDriverState opts = do
    prng <- case replay opts of
              Just (ReplaySplitmix seed gamma) ->
                return $ seedSMGen seed gamma
              Nothing ->
                initSMGen
    return $ DriverState {
        prng
      , successes        = []
      , todo             = tests opts
      , discardedForTest = 0
      , discardedTotal   = 0
      }

withSuccess :: SMGen -> Success a -> DriverState a -> DriverState a
withSuccess next success acc = DriverState {
      prng             = next
    , successes        = success : successes acc
    , todo             = pred (todo acc)
    , discardedForTest = 0 -- reset for the next test
    , discardedTotal   = discardedTotal acc
    }

withDiscard :: SMGen -> DriverState a -> DriverState a
withDiscard next acc = DriverState {
      prng             = next
    , successes        = successes acc
    , todo             = todo acc
    , discardedForTest = succ $ discardedForTest acc
    , discardedTotal   = succ $ discardedTotal acc
    }
