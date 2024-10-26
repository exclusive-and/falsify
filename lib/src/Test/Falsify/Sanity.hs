module Test.Falsify.Sanity (
    -- * Testing shrinking
    testShrinking
  , testMinimum
    -- * Testing generators
  , testGen
  , testGen'
  , testShrinkingOfGen
  ) where

import Prelude hiding (log)

import Control.Monad
import Control.Monad.State
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Test.Falsify.Gen (Gen)
import Test.Falsify.Gen            qualified as Gen
import Test.Falsify.Gen.Shrinking  qualified as Gen
import Test.Falsify.Gen.ShrinkTree qualified as Gen
import Test.Falsify.Predicate (Predicate, (.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Property
import Test.Falsify.Result
import Test.Falsify.Shrinking

{-------------------------------------------------------------------------------
  Test shrinking
-------------------------------------------------------------------------------}

-- | Test shrinking of a property
--
-- A property is normally only shrunk when it /fails/. We do the same here:
-- if the property succeeds, we discard the test and try again.
--
-- If the given property itself discards immediately, then this generator will
-- discard also; otherwise, only shrink steps are considered that do not lead
-- to a discard.
testShrinking :: forall e.
     Show e
  => Predicate [e, e] -> Property' e () -> Property' String ()
testShrinking p prop = do
    path <- genShrinkPath prop
    case findCounterExample (toList path) of
      Nothing ->
        return ()
      Just (err, logBefore, logAfter) -> do
        info "Before shrinking:"
        appendLog logBefore
        info "After shrinking:"
        appendLog logAfter
        testFailed err
  where
    findCounterExample :: [(e, TestRun)] -> Maybe (String, Log, Log)
    findCounterExample = \case
        []  -> Nothing
        [_] -> Nothing
        ((x, runX) : rest@((y, runY) : _)) ->
          case P.eval $ p .$ ("original", x) .$ ("shrunk", y) of
            Left err -> Just (err, runLog runX, runLog runY)
            Right () -> findCounterExample rest

-- | Construct random path through the property's shrink tree
genShrinkPath :: Property' e () -> Property' e' [(e, TestRun)]
genShrinkPath prop = do
    st    <- genWith (const Nothing) $ Gen.toShrinkTree (runProperty prop)
    mPath <- genWith (const Nothing) $ Gen.path resultIsValidShrink st
    aux mPath
  where
    aux ::
         Either (Maybe (), TestRun) (NonEmpty (e, TestRun))
      -> Property' e' [(e, TestRun)]
    aux (Left (Just (), _)) = return []
    aux (Left (Nothing, _)) = discard
    aux (Right es)          = return $ toList es

-- | Test the minimum error thrown by the property
--
-- If the given property passes, we will discard this test (in that case, there
-- is nothing to test); this test is also discarded if the given property
-- discards.
--
-- NOTE: When testing a particular generator, you might still want to test with
-- some particular property in mind. Otherwise, the minimum value will always
-- simply be the value that the generator produces when given the @Minimal@
-- sample tree.
testMinimum :: forall e.
     Show e
  => Predicate '[e]
  -> Property' e ()
  -> Property' String ()
testMinimum p prop = do
    st <- genWith (const Nothing) $ Gen.captureLocalTree
    case Gen.runGen (runProperty prop) st of
      ((TestPassed (), _run), _shrunk) ->
        -- The property passed; nothing to test
        discard
      ((TestDiscarded, _run), _shrunk) ->
        -- The property needs to be discarded; discard this one, too
        discard
      ((TestFailed initErr, initRun), shrunk) -> do
        let explanation :: ShrinkExplanation (e, TestRun) (Maybe (), TestRun)
            explanation = shrinkFrom
                            resultIsValidShrink
                            (runProperty prop)
                            ((initErr, initRun), shrunk)

            minErr    :: e
            minRun    :: TestRun
            mRejected :: Maybe [(Maybe (), TestRun)]
            ((minErr, minRun), mRejected) = shrinkOutcome explanation

            rejected :: [TestRun]
            rejected  = maybe [] (map snd) mRejected

        case P.eval $ p .$ ("minimum", minErr) of
          Right () -> do
            -- For a successful test, we add the full shrink history as info
            -- This means that users can use verbose mode to see precisely
            -- how the minimum value is reached, if they wish.
            info "Shrink history:"
            forM_ (shrinkHistory explanation) $ \(e, _run) ->
              info $ show e
          Left err -> do
            appendLog (runLog minRun)
            unless (null rejected) $ do
              info "\nLogs for rejected potential next shrinks:"
              forM_ (zip [0 :: Word ..] rejected) $ \(i, rej) -> do
                info $ "\n** Rejected run " ++ show i
                appendLog $ runLog rej
            testFailed err

{-------------------------------------------------------------------------------
  Testing generators
-------------------------------------------------------------------------------}

-- | Test output of the generator
testGen :: forall a. Show a => Predicate '[a] -> Gen a -> Property' String ()
testGen p = testGen' $ \a -> P.eval $ p .$ ("generated", a)

-- | Generalization of 'testGen'
testGen' :: forall e a b. (a -> Either e b) -> Gen a -> Property' e b
testGen' p g = WrapProperty $ TestResultT $ StateT $ \run ->
    -- We do not use bind here to avoid introducing new shrinking shortcuts
    aux run <$> g
  where
    aux :: TestRun -> a -> (TestResult e b, TestRun)
    aux run a = (
          case p a of
            Left  e -> TestFailed e
            Right b -> TestPassed b
        , run{runDeterministic = False}
        )

-- | Test shrinking of a generator
--
-- We check /any/ shrink step that the generator can make (independent of any
-- property).
testShrinkingOfGen :: Show a => Predicate [a, a] -> Gen a -> Property' String ()
testShrinkingOfGen p = testShrinking p . testGen' Left
