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
import qualified Language.Sunroof.JS.JQuery as JQuery
import qualified Language.Sunroof.JS.Browser as B

import Language.Sunroof.JS.Number
import Language.Sunroof.JS.String
import Language.Sunroof.JS.Bool
import Language.Sunroof.JS.Object
import Language.Sunroof.JS.Array as A
import Language.Sunroof.Classes

import System.Random
import System.IO
import System.Timeout
import Control.Concurrent.STM

import Data.Ratio

import Test.QuickCheck hiding ( assert )
import Test.QuickCheck.Monadic ( run, monadicIO, assert, pick, pre )
import qualified Test.QuickCheck.Monadic as M
import Test.QuickCheck.Gen ( Gen(MkGen, unGen) )
import Test.QuickCheck.Property hiding (Result,reason)
import qualified Test.QuickCheck.Property as P
--  ( callback, abort, ok
--  , Callback( PostTest )
--  , CallbackKind( NotCounterexample )
--  )
import Test.QuickCheck.State ( State( .. )) -- numSuccessTests ) )

import Control.Concurrent.ParallelIO.Local hiding (parallelInterleaved)
import Control.Concurrent.ParallelIO.Local (parallelInterleaved)
import qualified Control.Exception as E

main :: IO ()
main = sunroofServer (def { sunroofVerbose = 0, cometResourceBaseDir = ".." }) $ \ doc0 -> do
        let do_log = False
--        let te_style = TestWithTiming
        let te_style = TestInPar 4
        doc <- case te_style of
                  TestWithTiming -> newTimings doc0
                  _ -> return doc0
        web_app $ TestEngine doc do_log te_style False (5 * 1000 * 1000)

default(JSNumber, JSString, String)

type instance BooleanOf () = JSBool

data TestEngine = TestEngine { srEngine :: SunroofEngine
                             , teLog    :: Bool                 -- do you send information about each test to a log
                             , teStyle  :: TestStyle
                             , teShrink :: Bool                 -- do you want to shrink on failure?
                             , teTimeout :: Int                 -- millseconds timeout failure for each single test
                             }

data TestStyle = TestWithTiming         -- single core, do timing
               | TestInPar Int          -- How many cores

-- This is run each time the page is first accessed
web_app :: TestEngine -> IO ()
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

        runTests doc $ take 100 $ drop 0 $
          [ ("Constants",
                [ Test "Constant Numbers" (checkConstNumber doc :: Double -> Property)
                , Test "Constant Unit"    (checkConstValue doc :: () -> Property)
                , Test "Constant Boolean" (checkConstValue doc :: Bool -> Property)
                , Test "Constant String"  (checkConstValue doc :: String -> Property)
                ])
          , ("Arithmetic and Booleans",
                [ Test "Basic Addition"       (checkBasicArith doc (+) :: Double -> Double -> Property)
                , Test "Basic Subtraction"    (checkBasicArith doc (-) :: Double -> Double -> Property)
                , Test "Basic Multiplication" (checkBasicArith doc (*) :: Double -> Double -> Property)
                , Test "Arbitrary Arithmetic" (checkArbitraryArith doc)
                , Test "Arbitrary Boolean"    (checkArbitraryBool  doc)
                ])
          , ("Conditionals",
                [ Test "if/then/else -> Int (A)"   (checkArbitraryIfThenElse_Int doc tA)
                , Test "if/then/else -> Int (B)"   (checkArbitraryIfThenElse_Int doc tB)
                ])
          , ("Channels and MVars",
                [ Test "Chan (rand)"              (checkArbitraryChan_Int doc False SR.writeChan SR.readChan)
                , Test "Chan (write before read)" (checkArbitraryChan_Int doc True SR.writeChan SR.readChan)
                ])
          ]


-- -----------------------------------------------------------------------
-- Tests
-- -----------------------------------------------------------------------

-- | Check if a constant literal value is the same after sync.
checkConstValue :: ( Eq a
                   , SunroofValue a
                   , SunroofResult (ValueOf a)
                   , a ~ ResultOf (ValueOf a)
                   ) => TestEngine -> a -> Property
checkConstValue doc n = monadicIO $ do
  n' <- run $ sync (srEngine doc) (return $ js n)
  assert $ n == n'

-- | Check if a constant literal number is the same after sync.
checkConstNumber :: TestEngine -> Double -> Property
checkConstNumber doc n = monadicIO $ do
  n' <- run $ sync (srEngine doc) (return $ js n)
  -- Some weird conversion error going on. The returned value has more digits!
  assert $ n `deltaEqual` n'

-- | Check if simple arithmetic expressions with one operator produce
--   the same value after sync.
checkBasicArith :: TestEngine -> (forall b. (Num b) => b -> b -> b) -> Double -> Double -> Property
checkBasicArith doc op x y = monadicIO $ do
  let r = (x `op` y)
  r' <- run $ sync (srEngine doc) (return (js x `op` js y :: JSNumber))
  assert $ r `deltaEqual` r'

-- | Check if arithmetic expressions of arbitrary size produce the same result
--   after sync.
checkArbitraryArith :: TestEngine -> Int -> Property
checkArbitraryArith doc seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  (r, e) <- pick $ sameSeed (numExprGen n :: Gen Double)
                            (numExprGen n :: Gen JSNumber)
  pre $ abs r < (100000000 :: Double)
  r' <- run $ sync (srEngine doc) (return e)
  assert $ r `deltaEqual` r'

checkArbitraryBool :: TestEngine -> Int -> Property
checkArbitraryBool doc seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  (b, e) <- pick $ sameSeed (boolExprGen n :: Gen Bool)
                            (boolExprGen n :: Gen JSBool)
  b' <- run $ sync (srEngine doc) (return e)
  assert $ b == b'

checkArbitraryIfThenElse_Int :: forall t . (JSThread t) => TestEngine -> ThreadProxy t -> Int -> Property
checkArbitraryIfThenElse_Int doc ThreadProxy seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  (b, e) <- pick $ sameSeed (boolExprGen n :: Gen Bool)
                            (boolExprGen n :: Gen JSBool)
  (r1, e1) <- pick $ sameSeed (numExprGen n :: Gen Double)
                              (numExprGen n :: Gen JSNumber)
  (r2, e2) <- pick $ sameSeed (numExprGen n :: Gen Double)
                              (numExprGen n :: Gen JSNumber)
  pre $ abs r1 < (100000000 :: Double)
  pre $ abs r2 < (100000000 :: Double)
--  run $ print ("e,e1,e2",e,e1,e2)
  r12' <- run $ sync (srEngine doc) (ifB e (return e1) (return e2) >>= return :: JS t JSNumber)
  assert $ (if b then r1 else r2) == r12'

{-
checkArbitraryArray_Int
checkArbitraryArray_Int doc seed = monadicIO $ do
  let n = (abs seed `mod` 10) + 1
  sz <- pick $ choose (0,100)
  dat  :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector sz
-}


checkArbitraryChan_Int
        :: TestEngine
        -> Bool -- write before any read
        -> (JSNumber -> JSChan JSNumber -> JS B ())
        -> (JSChan JSNumber -> JS 'B JSNumber)
        -> Int
        -> Property
checkArbitraryChan_Int doc wbr writeChan readChan seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  qPush <- pick $ frequency [(1,return False),(3,return True)]
  qPull <- pick $ frequency [(1,return False),(3,return True)]
  arr1 :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 10
  arr2 :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 10
  dat  :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 10

  let prog :: JS B (JSArray JSNumber)
      prog = do
          note :: JSArray JSBool <- newArray
          ch <- SR.newChan
          (if wbr then id else forkJS) $
                   sequence_ [ do ifB (js (x >= 0 && qPush)) (threadDelayJSB (js x)) (return ())
                                  note # pushArray true
                                  ch # writeChan (js y :: JSNumber)
                             | (x,y) <- arr1 `zip` dat
                             ]
          arr :: JSArray JSNumber <- newArray
          sequence_ [ do ifB (js (x >= 0 && qPull)) (threadDelayJSB (js x)) (return ())
                         note # pushArray false
                         z <- ch # readChan
                         arr # pushArray z
                    | x <- arr2
                    ]

          when (teLog doc) $ do
                 -- debugging Glyph; perhaps send to Haskell-land,
                 -- or somehow print on the screen?
                 B.console # B.log (mconcat [ ifB (lookupArray (js n :: JSNumber) note)
                                                  (">"::JSString)
                                                  "<"
                                            | n <- [0..19::Int]
                                            ])


          return arr
  res :: [Double] <- run $ sync (srEngine doc) prog
  assert $ map round res == dat

-- -----------------------------------------------------------------------
-- Test execution
-- -----------------------------------------------------------------------

data Test = forall a. Testable a => Test String a

runTests :: TestEngine -> [(String,[Test])] -> IO ()
runTests doc all_tests = do
  sync (srEngine doc) $ do
          -- Set the fatal callback to continue, because we are testing things.
          fatal <- function $ \ (_::JSObject,_::JSObject,_::JSObject,f::JSFunction () ()) -> apply f ()
          () <- fun "$.kc.failure"  `apply` fatal
          return ()

  sequence_ [ do let
                     t  = "<h1>" ++ txt ++ "</h1>" ++
                          "<table>" ++ concat
                              [ "<tr class=\"" ++ pbName i j ++ "\"><td><div class=\"progressbar\"> </div></td><th>"
                                        ++ msg ++ "</th></tr>"
                              | (j::Int,Test msg _) <- [0..] `zip` tests
                              ] ++
                          "</table>"
                 async (srEngine doc) $ do
                         jQuery "#testing-text" >>= JQuery.append (cast $ js t)
                         return ()
           | (i::Int,(txt,tests)) <- [0..] `zip` all_tests
           ]

  -- set them all to 100 max
  async (srEngine doc) $ do
    () <- jQuery ".progressbar" >>= invoke "progressbar" ()  :: JS t ()
    () <- jQuery ".progressbar" >>= invoke "progressbar" ( "option" :: JSString
                                                   , "max" :: JSString
                                                   , 100 :: JSNumber
                                                   )
    () <- jQuery ".progressbar" >>= invoke "progressbar" ( "value" :: JSString
                                                   , 0 :: JSNumber
                                                   )
    return ()


  result <- (case teStyle doc of
                TestInPar n -> \ xs -> withPool n $ \ pool -> parallelInterleaved pool xs
                _ -> sequence) $ concat [
      [ do let casesPerTest :: Int
               casesPerTest = 100
               runTest :: Test -> IO Result
               runTest (Test name test) = do
                 putStrLn name
                 r <- quickCheckWithResult (stdArgs {chatty=False,maxSuccess=casesPerTest})
                   $ within (teTimeout doc)
                   $ (if teShrink doc then id else noShrinking)
                   $ callback afterTestCallback
                   $ test
                 print "DONE TESTS IN SR"
                 return r
               execTest :: Test -> IO ()
               execTest t@(Test name _) = do
--                 progressMsg doc name
                 result <- E.try (runTest t >>= E.evaluate)
                 case result of
                   Left (e ::  E.SomeException) -> do
                     print ("EXCEPTION:",e)
                     E.throw e
                   Right (Success _ _ out) -> do
                     putStrLn out
                   Right (GaveUp _ _ out) -> do
                     putStrLn out
                   Right f@(Failure {}) -> do
                     putStrLn (output f)
                     putStrLn (reason f)
                     putStrLn $ "FAILED TEST: " ++ name
                     appendMessage doc i j $ "(Failed)"
                     -- carry on, please
                     return ()
                   Right (NoExpectedFailure _ _ out) -> do
                     putStrLn out
               afterTestCallback :: Callback
               afterTestCallback = PostTest NotCounterexample $ \ state result -> do
                 if not (abort result) && isJust (ok result)
                   then do
                     progressVal doc i j (numSuccessTests state + 1)
                     if numSuccessTests state `mod` (casesPerTest `div` 10) == 0
                       then do
                         putStr "."
                         hFlush stdout
                       else return ()
                   else do
                     return ()
           execTest t

      | (j::Int,t@(Test msg _)) <- [0..] `zip` tests
      ]
    | (i::Int,(txt,tests)) <- [0..] `zip` all_tests
    ]


  return ()




pbName :: Int -> Int -> String
pbName i j = "pb-" ++ show i ++ "-" ++ show j

pbObject :: Int -> Int -> (String -> String) -> JS t JSObject
pbObject i j f = jQuery $ js $ f $ pbName i j

progressVal :: TestEngine -> Int -> Int -> Int -> IO ()
progressVal doc i j n = async (srEngine doc) $ do
  p <- pbObject i j $ \ n -> "." ++ n ++ " .progressbar"
  p # invoke "progressbar" ( "option" :: JSString
                           , "value" :: JSString
                           , js n :: JSNumber)


appendMessage :: TestEngine -> Int -> Int -> String -> IO ()
appendMessage doc i j msg = async (srEngine doc) $ do
  p <- pbObject i j $ \ n -> "." ++ n ++ " th"
  txt :: JSString <- p # invoke "html" ()
  p # JQuery.html(txt <> " " <> js msg)
  return ()

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


