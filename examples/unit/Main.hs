{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Prelude hiding (mod, div)

import Data.Monoid
import Data.Boolean
import Data.Boolean.Numbers hiding (floor, round)
import Data.Default
import Data.List
import Data.Char ( isControl, isAscii )
import Data.Maybe ( isJust )
import Data.Boolean

import Control.Concurrent

import Web.KansasComet hiding ( abort )
import qualified Web.KansasComet as KC

import Language.Sunroof as SR
import Language.Sunroof.KansasComet
import Language.Sunroof.JS.JQuery (jQuery)
import qualified Language.Sunroof.JS.Browser as B


import System.Random
import System.IO

import Data.Ratio

import Test.QuickCheck hiding ( assert )
import Test.QuickCheck.Monadic ( monadicIO, assert, run, pick, pre )
import Test.QuickCheck.Gen ( Gen(MkGen, unGen) )
import Test.QuickCheck.Property
  ( callback, abort, ok
  , Callback( PostTest )
  , CallbackKind( NotCounterexample )
  )
import Test.QuickCheck.State ( State( numSuccessTests ) )

main :: IO ()
main = sunroofServer (def { sunroofVerbose = 0, cometResourceBaseDir = ".." }) web_app

default(JSNumber, JSString, String)

type instance BooleanOf () = JSBool

data TestEngine = TestEngine { srEngine :: SunroofEngine
                             , teLog    :: Bool                 -- do you send information about each test to a log
                             }

-- This is run each time the page is first accessed
web_app :: SunroofEngine -> IO ()
web_app doc = do
        -- We use the lower-level waitForEvent, so we can test the JS compiler.
        {-
        forkIO $ do
                print "waiting"
                a <- waitForEvent (cometDocument doc) "session" abort
                print "waited"
                print a
        -}

        let tA = ThreadProxy :: ThreadProxy A
        let tB = ThreadProxy :: ThreadProxy B

        runTests doc $ drop 0 $
          [ Test "Constant Numbers" (checkConstNumber doc :: Double -> Property)
          , Test "Constant Unit"    (checkConstValue doc :: () -> Property)
          , Test "Constant Boolean" (checkConstValue doc :: Bool -> Property)
          , Test "Constant String"  (checkConstValue doc :: String -> Property)
          , Test "Basic Addition"       (checkBasicArith doc (+) :: Double -> Double -> Property)
          , Test "Basic Subtraction"    (checkBasicArith doc (-) :: Double -> Double -> Property)
          , Test "Basic Multiplication" (checkBasicArith doc (*) :: Double -> Double -> Property)
          , Test "Arbitrary Arithmetic" (checkArbitraryArith doc)
          , Test "Arbitrary Boolean"    (checkArbitraryBool  doc)
          , Test "if/then/else -> Int (A)"   (checkArbitraryIfThenElse_Int doc tA)
          , Test "if/then/else -> Int (B)"   (checkArbitraryIfThenElse_Int doc tB)
          , Test "Chan"                (checkArbitraryChan_Int doc)
          ]

-- -----------------------------------------------------------------------
-- Tests
-- -----------------------------------------------------------------------

-- | Check if a constant literal value is the same after sync.
checkConstValue :: ( Eq a
                   , SunroofValue a
                   , SunroofResult (ValueOf a)
                   , a ~ ResultOf (ValueOf a)
                   ) => SunroofEngine -> a -> Property
checkConstValue doc n = monadicIO $ do
  n' <- run $ sync doc (return $ js n)
  assert $ n == n'

-- | Check if a constant literal number is the same after sync.
checkConstNumber :: SunroofEngine -> Double -> Property
checkConstNumber doc n = monadicIO $ do
  n' <- run $ sync doc (return $ js n)
  -- Some weird conversion error going on. The returned value has more digits!
  assert $ n `deltaEqual` n'

-- | Check if simple arithmetic expressions with one operator produce
--   the same value after sync.
checkBasicArith :: SunroofEngine -> (forall b. (Num b) => b -> b -> b) -> Double -> Double -> Property
checkBasicArith doc op x y = monadicIO $ do
  let r = (x `op` y)
  r' <- run $ sync doc (return (js x `op` js y :: JSNumber))
  assert $ r `deltaEqual` r'

-- | Check if arithmetic expressions of arbitrary size produce the same result
--   after sync.
checkArbitraryArith :: SunroofEngine -> Int -> Property
checkArbitraryArith doc seed = monadicIO $ do
  let n = (abs seed `mod` 10) + 1
  (r, e) <- pick $ sameSeed (numExprGen n :: Gen Double)
                            (numExprGen n :: Gen JSNumber)
  pre $ abs r < (100000000 :: Double)
  r' <- run $ sync doc (return e)
  assert $ r `deltaEqual` r'

checkArbitraryBool :: SunroofEngine -> Int -> Property
checkArbitraryBool doc seed = monadicIO $ do
  let n = (abs seed `mod` 10) + 1
  (b, e) <- pick $ sameSeed (boolExprGen n :: Gen Bool)
                            (boolExprGen n :: Gen JSBool)
  b' <- run $ sync doc (return e)
  assert $ b == b'

checkArbitraryIfThenElse_Int :: forall t . (JSThread t) => SunroofEngine -> ThreadProxy t -> Int -> Property
checkArbitraryIfThenElse_Int doc ThreadProxy seed = monadicIO $ do
  let n = (abs seed `mod` 10) + 1
  (b, e) <- pick $ sameSeed (boolExprGen n :: Gen Bool)
                            (boolExprGen n :: Gen JSBool)
  (r1, e1) <- pick $ sameSeed (numExprGen n :: Gen Double)
                              (numExprGen n :: Gen JSNumber)
  (r2, e2) <- pick $ sameSeed (numExprGen n :: Gen Double)
                              (numExprGen n :: Gen JSNumber)
  pre $ abs r1 < (100000000 :: Double)
  pre $ abs r2 < (100000000 :: Double)
--  run $ print ("e,e1,e2",e,e1,e2)
  r12' <- run $ sync doc (ifB e (return e1) (return e2) >>= return :: JS t JSNumber)
  assert $ (if b then r1 else r2) == r12'


checkArbitraryChan_Int :: SunroofEngine -> Int -> Property
checkArbitraryChan_Int doc seed = monadicIO $ do
  let n = (abs seed `mod` 10) + 1
  qPush <- pick $ frequency [(1,return False),(3,return True)]
  qPull <- pick $ frequency [(1,return False),(3,return True)]
  arr1 :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 10
  arr2 :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 10
  dat  :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 10

  let prog :: JS B (JSArray JSNumber)
      prog = do
          note :: JSArray JSBool <- newArray
          ch <- SR.newChan
          forkJS $ sequence_ [ do ifB (js (x >= 0 && qPush)) (threadDelayJSB (js x)) (return ())
                                  note # pushArray true
                                  ch # SR.writeChan (js y :: JSNumber)
                             | (x,y) <- arr1 `zip` dat
                             ]
          arr :: JSArray JSNumber <- newArray
          sequence_ [ do ifB (js (x >= 0 && qPull)) (threadDelayJSB (js x)) (return ())
                         note # pushArray false
                         z <- ch # SR.readChan
                         arr # pushArray z
                    | x <- arr2
                    ]

{-
          -- debugging Glyph; perhaps send to Haskell-land,
          -- or somehow print on the screen?
          B.console # B.log (mconcat [ ifB (lookupArray (js n :: JSNumber) note)
                                                (">"::JSString)
                                                "<"
                                     | n <- [0..19::Int]
                                     ])

-}
          return arr
  res :: [Double] <- run $ sync doc prog
  assert $ map round res == dat

-- -----------------------------------------------------------------------
-- Test execution
-- -----------------------------------------------------------------------

data Test = forall a. Testable a => Test String a

runTests :: SunroofEngine -> [Test] -> IO ()
runTests doc tests = do
  let testCount = length tests
  progressMax doc (testCount * casesPerTest)
  progressVal doc 0
  execTests tests
  where
    casesPerTest :: Int
    casesPerTest = 100
    runTest :: Test -> IO Result
    runTest (Test name test) = do
      putStrLn name
      quickCheckWithResult (stdArgs {chatty=False,maxSuccess=casesPerTest})
        $ callback afterTestCallback
        $ test
    execTests :: [Test] -> IO ()
    execTests [] = do
      putStrLn "PASSED ALL TESTS"
      progressMsg doc "PASSED ALL TESTS"
    execTests (t@(Test name _):ts) = do
      progressMsg doc name
      result <- runTest t
      case result of
        Success _ _ out -> do
          putStrLn out
          execTests ts
        GaveUp _ _ out -> do
          putStrLn out
          execTests ts
        Failure _ _ _ _ reason _ out -> do
          putStrLn out
          putStrLn reason
          putStrLn $ "FAILED TEST: " ++ name
        NoExpectedFailure _ _ out -> do
          putStrLn out
          execTests ts
    afterTestCallback :: Callback
    afterTestCallback = PostTest NotCounterexample $ \ state result -> do
      if not (abort result) && isJust (ok result)
        then do
          progressInc doc
          if numSuccessTests state `mod` (casesPerTest `div` 10) == 0
            then do
              putStr "."
              hFlush stdout
            else return ()
        else do
          return ()

progressMax :: SunroofEngine -> Int -> IO ()
progressMax doc n = async doc $ do
  p <- jQuery "#progressbar"
  p # invoke "progressbar" ( "option" :: JSString
                           , "max" :: JSString
                           , js n :: JSNumber)

progressVal :: SunroofEngine -> Int -> IO ()
progressVal doc n = async doc $ do
  p <- jQuery "#progressbar"
  p # invoke "progressbar" ( "option" :: JSString
                           , "value" :: JSString
                           , js n :: JSNumber)

progressMsg :: SunroofEngine -> String -> IO ()
progressMsg doc msg = async doc $ do
  l <- jQuery "#plabel"
  l # invoke "text" (js msg :: JSString)

progressInc :: SunroofEngine -> IO ()
progressInc doc = async doc $ do
  p <- jQuery "#progressbar"
  n <- p # invoke "progressbar" ( "option" :: JSString
                                , "value" :: JSString)
  p # invoke "progressbar" ( "option" :: JSString
                           , "value" :: JSString
                           , n + 1 :: JSNumber)

-- -----------------------------------------------------------------------
-- Test Utilities
-- -----------------------------------------------------------------------

-- | Look if two fractional values are almost the same.
deltaEqual :: (Ord a, Fractional a) => a -> a -> Bool
deltaEqual x y = x >= y - delta && x <= y + delta
  where delta = 0.00000000000001

-- | Use to generators with the same seed and size.
--   This is useful for overloaded value generation.
--   Example:
--
-- > sameSeed (numGen :: Gen Double) (numGen :: Gen JSNumber)
--
--   Both generators will produce the same overloaded value that can be casted
--   to the appropriate type.
sameSeed :: Gen a -> Gen b -> Gen (a,b)
sameSeed genA genB = MkGen $ \gen size -> (unGen genA gen size, unGen genB gen size)

-- -----------------------------------------------------------------------
-- Custom Generators
-- -----------------------------------------------------------------------

instance Arbitrary JSNumber where
  arbitrary = numGen

instance Arbitrary JSBool where
  arbitrary = fmap js (arbitrary :: Gen Bool)

instance Arbitrary JSString where
  arbitrary = fmap js (arbitrary :: Gen String)

numGen :: (Num b) => Gen b
numGen = do
  n <- arbitrary :: Gen Integer
  return $ fromIntegral $ (n `Prelude.rem` 10000)

numExprGen :: Num a => Int -> Gen a
numExprGen 0 = numGen
numExprGen n = frequency [(1, numGen), (2, binaryGen)]
  where binaryGen :: Num a => Gen a
        binaryGen = do
          op <- elements [(+),(-),(*)]
          e1 <- numExprGen $ n - 1
          e2 <- numExprGen $ n - 1
          return $ e1 `op` e2

{-
eqExprGen :: (EqB a) => Gen a -> Gen (BooleanOf a)
eqExprGen genA = do
  op <- elements [(==*),(/=*)]
  e1 <- genA
  e2 <- genA
  return $ e1 `op` e2

ordExprGen :: (OrdB a) => Gen a -> Gen (BooleanOf a)
ordExprGen genA = do
  op <- elements [(<=*),(>=*),(<*),(>*)]
  e1 <- genA
  e2 <- genA
  return $ e1 `op` e2
-}

boolGen :: (Boolean b) => Gen b
boolGen = elements [true, false]

boolExprGen :: (Boolean b, b ~ BooleanOf b, EqB b, IfB b) => Int -> Gen b
boolExprGen 0 = boolGen
boolExprGen n = frequency [(1, boolGen), (3, binaryGen), (1, ifGen), (1, unaryGen)]
  where binaryGen :: (Boolean b, b ~ BooleanOf b, EqB b, IfB b) => Gen b
        binaryGen = do
          op <- elements [(&&*),(||*),(==*),(/=*)]
          e1 <- boolExprGen $ n - 1
          e2 <- boolExprGen $ n - 1
          return $ e1 `op` e2
        ifGen :: (Boolean b, b ~ BooleanOf b, EqB b, IfB b) => Gen b
        ifGen = do
          e1 <- boolExprGen $ n - 1
          e2 <- boolExprGen $ n - 1
          e3 <- boolExprGen $ n - 1
          return $ ifB e1 e2 e3
        unaryGen :: (Boolean b, b ~ BooleanOf b, EqB b, IfB b) => Gen b
        unaryGen = do
          e1 <- boolExprGen $ n - 1
          return $ notB e1


{-
data Op2 = Op2 (forall a . Num a => a -> a -> a) String
op2s = [ Op2 (+) "+", Op2 (-) "-", Op2 (*) "*"]

newtype Arb a = Arb { unArb :: StdGen -> a }

finite :: [a] -> Arb a
finite xs = Arb $ \ rnd -> xs !! fst (randomR (0,len - 1) rnd)
  where len =  length xs

choice :: Rational -> Arb a -> Arb a -> Arb a
choice r a1 a2 | numerator r > denominator r  = error "ratio > 1"
               | numerator r < 0              = error "ratio < 0"
               | numerator r == denominator r = a1
               | numerator r == 0             = a2
               | otherwise                    = Arb $ \ rnd ->
                 let (x,rnd') = randomR (0,denominator r) rnd
                 in if numerator r > x
                    then unArb a1 rnd'
                    else unArb a2 rnd'

instance Functor Arb where
        fmap f (Arb arb) = Arb (f . arb)

instance Monad Arb where
        return a = Arb (const a)
        Arb m >>= k = Arb $ \ std ->
                let (stdA,stdB) = split std
                in unArb (k (m stdA)) stdB

instance Applicative Arb where
        pure = return
        (<*>) = ap

arbFromIntegral :: (Num b) => Arb  b
arbFromIntegral = Arb $ \ std ->
   let n :: Integer = (`Prelude.rem` 10000) $ fst $ random std
   in fromIntegral n

expr :: Num b => Int -> Arb b
expr 0 = arbFromIntegral
expr n = choice 0.5
        (arbFromIntegral)
        (do o <- finite [(+),(-),(*)]
            e1 <- expr (n-1)
            e2 <- expr (n-1)
            return (o e1 e2))
-}