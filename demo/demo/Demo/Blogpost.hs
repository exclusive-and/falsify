module Demo.Blogpost where

import Control.Monad
import Control.Monad.State
import Control.Selective
import Data.Bifunctor
import Data.Default
import Data.Word
import System.Random.SplitMix (SMGen)
import System.Random.SplitMix qualified as SplitMix
import Test.Falsify
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Prelude hiding (elem)
import Test.Falsify.Sanity
import Test.Tasty
import Test.Tasty.Falsify
import Test.Tasty.HUnit (Assertion, testCase, assertFailure)

tests :: TestTree
tests = testGroup "Demo.Blogpost" [
      testProperty "motivation" prop_list
    , testGroup "Background" [
        testGroup "UnitVsPBT" [
              testCase "unit" $
                unit_structure
                  even
                  (3 :: Word)
                  False
            , testProperty "property" $
               prop_structure
                 (reverse . reverse)
                 (list (between (0, 100)) (bool False))
                 (==)
            ]
        ]
    , testGroup "ImportanceOfShrinking" [
          testPropertyWith (def { overrideMaxShrinks = Just 0 })
            "shrinking_disabled" prop_shrinking
        , testProperty
            "shrinking_enabled" prop_shrinking
        ]
    , testGroup "Tutorial" [
          testProperty "multiply2_even"      prop_multiply2_even
        , testProperty "multiply3_even"      prop_multiply3_even
        , testProperty "multiply3_even_pred" prop_multiply3_even_pred
        , testGroup "skew" [
              testProperty "0" $ prop_skew 0
            , testProperty "5" $ prop_skew 5
            ]
        , testProperty "fn1"               prop_fn
        , testProperty "mapFilter"         prop_mapFilter
        , testProperty "below_shrinking"   prop_below_shrinking
        , testProperty "naiveList_minimum" prop_naiveList_minimum
        , testProperty "list_minimum"      prop_list_minimum
        ]
    ]

{-------------------------------------------------------------------------------
  Motivating example
-------------------------------------------------------------------------------}

-- | Example of interesting shrinking across monadic bind
--
-- Run with this seed for a good example:
--
-- > cabal run demo -- \
-- >    -p Blogpost.motivation \
-- >    --falsify-verbose \
-- >    --falsify-replay=012f1ef548663a9b73ceaebf948d9f87a7
prop_list :: Property ()
prop_list = do
    n  <- gen $ inRange $ between (0, 10)
    xs <- gen $ replicateM n $ int $ between (0, 1)
    assert $ P.pairwise P.eq .$ ("xs", xs)

{-------------------------------------------------------------------------------
  Background: unit testing versus PBT
-------------------------------------------------------------------------------}

unit_structure :: Eq b => (a -> b) -> a -> b -> Assertion
unit_structure f input expected =
    unless (f input == expected) $
      assertFailure "not equal"

prop_structure :: Show a => (a -> b) -> Gen a -> (a -> b -> Bool) -> Property ()
prop_structure f genInput prop = do
    input <- gen $ genInput
    unless (prop input (f input)) $
      testFailed "property not satisfied"

{-------------------------------------------------------------------------------
  Background: importance of shrinking
-------------------------------------------------------------------------------}

prop_shrinking :: Property ()
prop_shrinking = do
    x <- gen $ int $ between (0, 99)
    y <- gen $ int $ between (0, 99)
    unless (x - y == y - x) $
      testFailed "property not satisfied"

{-------------------------------------------------------------------------------
  Parsing versus generation
-------------------------------------------------------------------------------}

type PRNG = SMGen

next :: PRNG -> (Word, PRNG)
next = first fromIntegral . SplitMix.nextWord64

newtype LinearGen a = LinearGen (State PRNG a)
  deriving newtype (Functor, Applicative, Monad)

runLinearGen :: LinearGen a -> PRNG -> (a, PRNG)
runLinearGen (LinearGen g) = runState g

unfoldLinear :: PRNG -> [Word]
unfoldLinear prng =
    let (s, prng') = next prng
    in s : unfoldLinear prng'

newtype LinearParser a = LinearParser (State [Word] a)
  deriving newtype (Functor, Applicative, Monad)

runLinearParser :: LinearParser a -> [Word] -> (a, [Word])
runLinearParser (LinearParser g) = runState g

parseBool :: LinearParser Bool
parseBool = LinearParser $ state $ \case
    []     -> error "parseBool: no more samples"
    (s:ss) -> (
        if s >= maxBound `div` 2 then True else False
      , ss
      )

{-------------------------------------------------------------------------------
  QuickCheck
-------------------------------------------------------------------------------}

newtype QcGen a = QcGen (PRNG -> a)
  deriving (Functor)

split :: PRNG -> (PRNG, PRNG)
split = SplitMix.splitSMGen

bothQc :: QcGen a -> QcGen b -> QcGen (a, b)
bothQc (QcGen g1) (QcGen g2) = QcGen $ \prng ->
    let (l, r) = split prng
    in (g1 l, g2 r)

instance Applicative QcGen where
  pure x  = QcGen $ \_ -> x
  f <*> x = uncurry ($) <$> bothQc f x

{-------------------------------------------------------------------------------
  Falsify
-------------------------------------------------------------------------------}

data STree = STree Word STree STree

unfoldTree :: PRNG -> STree
unfoldTree prng =
    let (s, _) = next  prng
        (l, r) = split prng
    in STree s (unfoldTree l) (unfoldTree r)

newtype FalsifyGen a = FalsifyGen (STree -> (a, [STree]))

bothFalsify :: FalsifyGen a -> FalsifyGen b -> FalsifyGen (a, b)
bothFalsify (FalsifyGen g1) (FalsifyGen g2) = FalsifyGen $ \(STree s l r) ->
    let (a, ls) = g1 l
        (b, rs) = g2 r
    in ( (a, b)
       ,    [STree s l' r  | l' <- ls]
         ++ [STree s l  r' | r' <- rs]
       )

{-------------------------------------------------------------------------------
  Consequences of using sample trees
-------------------------------------------------------------------------------}

incomparableTrees :: (STree, STree)
incomparableTrees = (
      STree undefined
        (STree 1 undefined undefined)
        (STree 4 undefined undefined)

    , STree undefined
        (STree 2 undefined undefined)
        (STree 3 undefined undefined)
    )

{-------------------------------------------------------------------------------
  Predictability
-------------------------------------------------------------------------------}

listThenNum :: Gen ([Bool], Int)
listThenNum = do
    xs <- list (between (0, 100)) $ bool False
    n  <- int (between (0, 100))
    return (xs, n)

{-------------------------------------------------------------------------------
  Selective functors
-------------------------------------------------------------------------------}

chooseSuboptimal :: Gen a -> Gen a -> Gen a
chooseSuboptimal g g' = do
    b <- bool True
    if b then g else g'

chooseBad :: Gen a -> Gen a -> Gen a
chooseBad g g' = do
    x <- g
    y <- g'
    b <- bool True
    return $ if b then x else y

choose :: Gen a -> Gen a -> Gen a
choose = ifS $ bool True

{-------------------------------------------------------------------------------
  Tutorial
-------------------------------------------------------------------------------}

prop_multiply2_even :: Property ()
prop_multiply2_even = do
    x <- gen $ int $ withOrigin (-100, 100) 0
    unless (even (x * 2)) $ testFailed "not even"

prop_multiply3_even :: Property ()
prop_multiply3_even = do
    x <- gen $ int $ withOrigin (-100, 100) 0
    unless (even (x * 3)) $ testFailed "not even"

prop_multiply3_even_pred :: Property ()
prop_multiply3_even_pred = do
    x <- gen $ int $ withOrigin (-100, 100) 0
    assert $ P.even `P.dot` P.fn ("multiply3", (* 3)) .$ ("x", x)

prop_skew :: Double -> Property ()
prop_skew skew = do
    xs <- gen $ list rangeListLen $ inRange rangeValues
    x  <- gen $ inRange rangeValues
    collect "elem" [x `elem` xs]
  where
    rangeListLen, rangeValues :: Range Word
    rangeListLen = between (0, 10)
    rangeValues  = skewedBy skew (0, 100)

prop_fn :: Property ()
prop_fn = do
    Fn (f :: [Int] -> Bool) <- gen $ fun $ bool False
    assert $
         P.eq
         `P.on` P.fn ("f", f)
      .$ ("x", [1, 2, 3])
      .$ ("y", [4, 5, 6])

prop_mapFilter :: Property ()
prop_mapFilter = do
    Fn (f :: Int -> Int)  <- gen $ fun genInt
    Fn (p :: Int -> Bool) <- gen $ fun genBool
    xs :: [Int] <- gen $ list (between (0, 100)) genInt
    assert $
       P.eq
      `P.split` (P.fn ("map f", map f), P.fn ("filter p", filter p))
      `P.split` (P.fn ("filter p", filter p), P.fn ("map f", map f))
      .$ ("xs", xs)
      .$ ("xs", xs)
  where
    genInt :: Gen Int
    genInt = int $ between (0, 100)

    genBool :: Gen Bool
    genBool = bool False

{-------------------------------------------------------------------------------
  Testing shrinking
-------------------------------------------------------------------------------}

below :: Word64 -> Gen Word64
below n = (`mod` n) <$> prim

prop_below_shrinking :: Property ()
prop_below_shrinking = do
    n <- gen $ inRange $ between (1, 1_000)
    testShrinkingOfGen P.ge $ below n

naiveList :: Range Int -> Gen a -> Gen [a]
naiveList r g = do
    n  <- inRange r
    replicateM n g

prop_naiveList_minimum :: Property ()
prop_naiveList_minimum =
    testMinimum (P.elem .$ ("expected", [[0,1], [1,0]])) $ do
      xs <- gen $ naiveList
                    (between (0, 10))
                    (int (between (0, 1)))
      case P.eval $ P.pairwise P.eq .$ ("xs", xs) of
        Left _   -> testFailed xs
        Right () -> return ()

prop_list_minimum :: Property ()
prop_list_minimum =
    testMinimum (P.elem .$ ("expected", [[0,1], [1,0]])) $ do
      xs <- gen $ list
                    (between (0, 10))
                    (int (between (0, 1)))
      case P.eval $ P.pairwise P.eq .$ ("xs", xs) of
        Left _   -> testFailed xs
        Right () -> return ()
