module Test.Falsify.Pretty (
      renderTestResult
    , Verbose (..)
    , ExpectFailure (..)
    , RenderedTestResult (..)
    ) where

import Prelude hiding (log)

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exception
import Test.Falsify
import Test.Falsify.ReplaySeed
import Test.Falsify.Result
import Test.Falsify.Shrinking
import Text.Printf

-- | Verbose output
--
-- Note that if a test fails (and we were not expecting failure) we show the
-- logs independent of verbosity.
data Verbose = Verbose | NotVerbose

-- | Do we expect the property to fail?
--
-- If 'ExpectFailure', the test will fail if the property does /not/ fail.
-- Note that if we expect failure for a property, then we can stop at the first
-- failed test; the number of tests to run for the property becomes a maximum
-- rather than a goal.
data ExpectFailure = ExpectFailure | DontExpectFailure

-- | Test result as it should be shown to the user
data RenderedTestResult = RenderedTestResult {
      testPassed :: Bool
    , testOutput :: String
    }

renderTestResult ::
     Verbose
  -> ExpectFailure
  -> (ReplaySeed, [Success ()], TotalDiscarded, Maybe (Failure String))
  -> RenderedTestResult
renderTestResult
      verbose
      expectFailure
      (initSeed, successes, TotalDiscarded discarded, mFailure) =
    case (verbose, expectFailure, mFailure) of

      --
      -- All tests discarded
      --
      -- TODO: Verbose mode here does nothing currently (we get no logs for
      -- discarded tests).
      --

      (_, DontExpectFailure, Nothing) | null successes -> RenderedTestResult {
            testPassed = False
          , testOutput = unlines [
                concat [
                    "All tests discarded"
                  , countDiscarded
                  ]
              ]
          }

      --
      -- Test succeeded
      --
      -- This may still be a failure, if we were expecting the test not to
      -- succeed.
      --

      (NotVerbose, DontExpectFailure, Nothing) -> RenderedTestResult {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     countSuccess
                   , countDiscarded
                   ]
               , showLabels
               ]
           }

      (Verbose, DontExpectFailure, Nothing) -> RenderedTestResult {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     countSuccess
                   , countDiscarded
                   ]
               , ""
               , "Logs for each test run below."
               , ""
               , unlines $ map renderSuccess (zip [1..] successes)
               ]
           }

      (NotVerbose, ExpectFailure, Nothing) -> RenderedTestResult {
             testPassed = False
           , testOutput = unlines [
                 "Expected failure, but " ++ countAll ++ " passed"
               , showSeed initSeed
               ]
           }

      (Verbose, ExpectFailure, Nothing) -> RenderedTestResult {
             testPassed = False
           , testOutput = unlines [
                 "Expected failure, but " ++ countAll ++ " passed"
               , ""
               , "Logs for each test run below."
               , ""
               , intercalate "\n" $ map renderSuccess (zip [1..] successes)
               , showSeed initSeed
               ]
           }

      --
      -- Test failed
      --
      -- This might still mean the test passed, if we /expected/ failure.
      --
      -- If the test failed and we were not expecting failure, we show the
      -- logs independent of verbosity.
      --

      (NotVerbose, ExpectFailure, Just e) -> RenderedTestResult {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     "expected failure after "
                   , countHistory history
                   , countDiscarded
                   ]
               , fst $ NE.last history
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (Verbose, ExpectFailure, Just e) -> RenderedTestResult {
             testPassed = True
           , testOutput = unlines [
                 concat [
                     "expected failure after "
                   , countHistory history
                   , countDiscarded
                   ]
               , fst $ NE.last history
               , "Logs for failed test run:"
               , renderLog . runLog . snd $ NE.last history
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (NotVerbose, DontExpectFailure, Just e) -> RenderedTestResult {
             testPassed = False
           , testOutput = unlines [
                 "failed after " ++ countHistory history
               , fst $ NE.last history
               , "Logs for failed test run:"
               , renderLog . runLog . snd $ NE.last history
               , showSeed $ failureSeed e
               ]
           }
         where
           history = shrinkHistory (failureRun e)

      (Verbose, DontExpectFailure, Just e) -> RenderedTestResult {
             testPassed = False
           , testOutput = unlines [
                 "failed after " ++ countHistory history
               , fst $ NE.last history
               , ""
               , "Logs for complete shrink history:"
               , ""
               , intercalate "\n" $ [
                     intercalate "\n" [
                         "Step " ++ show (step :: Word)
                       , renderLog (runLog run)
                       ]
                   | (step, (_result, run)) <- zip [1..] (NE.toList history)
                   ]
               , showSeed $ failureSeed e
               ]
           }
         where
           history = shrinkHistory (failureRun e)
  where
    countSuccess, countDiscarded, countAll :: String
    countSuccess
      | length successes == 1 = "1 successful test"
      | otherwise             = show (length successes) ++ " successful tests"
    countDiscarded
      | discarded == 0        = ""
      | otherwise             = " (discarded " ++ show discarded ++ ")"
    countAll
      | length successes == 1 = "the test"
      | otherwise             = "all " ++ show (length successes) ++ " tests"

    -- The history includes the original value, so the number of shrink steps
    -- is the length of the history minus 1.
    countHistory :: NonEmpty (String, TestRun) -> [Char]
    countHistory history = concat [
          if | length successes == 0 -> ""
             | otherwise             -> countSuccess ++ " and "
        , if | length history   == 2 -> "1 shrink"
             | otherwise             -> show (length history - 1) ++ " shrinks"
        ]

    showSeed :: ReplaySeed -> String
    showSeed seed = "Use --falsify-replay=" ++ show seed ++ " to replay."

    showLabels :: String
    showLabels = intercalate "\n" [
          intercalate "\n" $ ("\nLabel " ++ show l ++ ":") : [
              asPct n ++ " " ++ v
            | v <- Set.toList (Map.findWithDefault Set.empty l allValues)
            , let n = Map.findWithDefault 0         v
                    $ Map.findWithDefault Map.empty l
                    $ perTest
            ]
        | l <- Set.toList allLabels
        ]
      where
        -- Absolute number of tests as a percentage of total successes
        asPct :: Int -> String
        asPct n =
           printf "  %8.4f%%" pct
          where
            pct :: Double
            pct = fromIntegral n / fromIntegral (length successes) * 100

        -- All labels across all tests
        allLabels :: Set String
        allLabels = Map.keysSet allValues

        -- For each label, all values reported across all tests
        allValues :: Map String (Set String)
        allValues =
            Map.unionsWith Set.union $
              map (runLabels . successRun) successes

        -- For each label and each value, the corresponding number of tests
        perTest :: Map String (Map String Int)
        perTest =
            Map.fromList [
                (l, Map.fromList [
                    (v, length $ filter (labelHasValue l v) successes)
                  | v <- Set.toList $
                             Map.findWithDefault Set.empty l allValues
                  ])
              | l <- Set.toList allLabels
              ]

        -- Check if in particular test run label @l@ has value @v@
        labelHasValue :: String -> String -> Success () -> Bool
        labelHasValue l v =
              Set.member v
            . Map.findWithDefault Set.empty l
            . runLabels
            . successRun

renderSuccess :: (Int, Success ()) -> String
renderSuccess (ix, Success{successRun}) =
    intercalate "\n" . concat $ [
        ["Test " ++ show ix]
      , [renderLog $ runLog successRun]
      ]

renderLog :: Log -> String
renderLog (Log log) = unlines $ map renderLogEntry (reverse log)

renderLogEntry :: LogEntry -> String
renderLogEntry = \case
    Generated stack x -> concat [
        "generated "
      , x
      , " at "
      , prettyCallStack stack
      ]
    Info x -> x
