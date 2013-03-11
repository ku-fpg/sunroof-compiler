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

import Data.Semigroup
import Data.Boolean
import Data.Boolean.Numbers hiding (floor, round)
import Data.Default
import Data.List
import Data.Char ( isControl, isAscii )
import Data.Maybe ( isJust )
import Data.Boolean

import qualified Numeric

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
import Control.Monad (when, liftM2)

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
        let te_style = TestWithTiming
--        let te_style = TestInPar 20
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
               deriving (Show, Eq)

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
                [ Test 100 "Constant Numbers" (checkConstNumber doc :: Double -> Property)
                , Test 100 "Constant Unit"    (checkConstValue doc :: () -> Property)
                , Test  10 "Constant Boolean" (checkConstValue doc :: Bool -> Property)
                , Test 100 "Constant String"  (checkConstValue doc :: String -> Property)
                ])
          , ("Arithmetic and Booleans",
                [ Test 100 "Basic Addition"       (checkBasicArith doc (+) :: Double -> Double -> Property)
                , Test 100 "Basic Subtraction"    (checkBasicArith doc (-) :: Double -> Double -> Property)
                , Test 100 "Basic Multiplication" (checkBasicArith doc (*) :: Double -> Double -> Property)
                , Test 100 "Arbitrary Arithmetic" (checkArbitraryArith doc)
                , Test 100 "Arbitrary Boolean"    (checkArbitraryBool  doc)
                ])
          , ("Conditionals",
                [ Test  10 "if/then/else -> Int (A)"   (checkArbitraryIfThenElse_Int doc tA)
                , Test  10 "if/then/else -> Int (B)"   (checkArbitraryIfThenElse_Int doc tB)
                ])
          , ("Channels and MVars",
                [ Test  10 "Chan (rand)"              (checkArbitraryChan_Int doc False SR.newChan SR.writeChan SR.readChan)
                , Test  10 "Chan (write before read)" (checkArbitraryChan_Int doc True SR.newChan SR.writeChan SR.readChan)
                , Test  10 "MVar (rand)"              (checkArbitraryChan_Int doc False SR.newEmptyMVar SR.putMVar SR.takeMVar)
                ])
          , ("Performance",
                [ Test   1 ("Fib " ++ show n)           (runFib doc n) | n <- [10,      30]
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
        -> (JS B (m JSNumber))
        -> (JSNumber -> m JSNumber -> JS B ())
        -> (m JSNumber -> JS 'B JSNumber)
        -> Int
        -> Property
checkArbitraryChan_Int doc wbr newChan writeChan readChan seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  qPush <- pick $ frequency [(1,return False),(3,return True)]
  qPull <- pick $ frequency [(1,return False),(3,return True)]
  arr1 :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 5
  arr2 :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 5
  dat  :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 5

  let prog :: JS B (JSArray JSNumber)
      prog = do
          note :: JSArray JSBool <- newArray ()
          ch <- newChan
          (if wbr then id else forkJS) $
                   sequence_ [ do ifB (js (x >= 0 && qPush)) (SR.threadDelay (js x)) (return ())
                                  note # A.push true
                                  ch # writeChan (js y :: JSNumber)
                             | (x,y) <- arr1 `zip` dat
                             ]
          arr :: JSArray JSNumber <- newArray ()
          sequence_ [ do ifB (js (x >= 0 && qPull)) (SR.threadDelay (js x)) (return ())
                         note # A.push false
                         z <- ch # readChan
                         arr # A.push z
                    | x <- arr2
                    ]

          when (teLog doc) $ do
                 -- debugging Glyph; perhaps send to Haskell-land,
                 -- or somehow print on the screen?
                 B.console # B.log (mconcat [ ifB (A.lookup (js n :: JSNumber) note)
                                                  (">"::JSString)
                                                  "<"
                                            | n <- [0..19::Int]
                                            ])


          return arr
  res :: [Double] <- run $ sync (srEngine doc) prog
  assert $ map round res == dat


-- | Check if simple arithmetic expressions with one operator produce
--   the same value after sync.
runFib :: TestEngine -> Int -> Property
runFib doc n = monadicIO $ do
  r' <- run $ sync (srEngine doc) $ do
        fib <- function $ fixJS $ \ fib (n :: JSNumber) -> do
                ifB (n <* 2)
                    (return (1 :: JSNumber))
                    (liftM2 (+) (fib (n - 1)) (fib (n - 2)))
        apply fib (js n)
  let fib :: Int -> Int
      fib n = xs !! n
      xs = map (\ n -> if n < 2 then 1 else fib (n-1) + fib (n-2)) [0..]
  let r = fromIntegral (fib n)
  assert $ r `deltaEqual` r'


-- Needs to be in Utils module
fixJS :: (JSArgument a, Sunroof b) => ((a -> JSA b) -> (a -> JSA b)) -> a -> JSA b
fixJS f a = do
        ref <- newJSRef (cast nullJS)
        fn <- function $ \ a' -> do
                        fn' <- readJSRef ref
                        f (apply fn') a'
        writeJSRef ref fn
        apply fn a

-- -----------------------------------------------------------------------
-- Test execution
-- -----------------------------------------------------------------------

data Test = forall a. Testable a => Test Int String a

runTests :: TestEngine -> [(String,[Test])] -> IO ()
runTests doc all_tests = do
  sync (srEngine doc) $ do
          -- Set the fatal callback to continue, because we are testing things.
          fatal <- function $ \ (_::JSObject,_::JSObject,_::JSObject,f::JSFunction () ()) -> apply f ()
          () <- fun "$.kc.failure"  `apply` fatal
          return ()


  let section title body = do
          async (srEngine doc) $ do
                   jQuery "#testing-text" >>= JQuery.append (cast $ js $
                          "<h1>" ++ title ++ "</h1>" ++ "<table>" ++ body ++ "</table>")
                   return ()

  sequence_ [ do section txt $ concat
                              [ "<tr class=\"" ++ pbName i j ++ "\"><td class=\"progress\"><div class=\"progressbar\"> </div></td><th>"
                                        ++ msg ++ "</th>" ++
                                                "<td class=\"data\"></td>" ++
                                                "<td class=\"data\"></td>" ++
                                                "<td class=\"data\"></td>" ++
                                                "</tr>"
                              | (j::Int,Test _ msg _) <- [1..] `zip` tests
                              ]
           | (i::Int,(txt,tests)) <- [1..] `zip` all_tests
           ]

  section "Summary" $ "<tr class=\"" ++ pbName 0 0 ++ "\"><td class=\"progress\"></td><th></th>" ++
                                                "<td class=\"data\"></td>" ++
                                                "<td class=\"data\"></td>" ++
                                                "<td class=\"data\"></td>" ++
                                                "</tr>"

  let casesPerTest :: Int
      casesPerTest = 100

  -- set them all to 100 max
  async (srEngine doc) $ do
    () <- jQuery ".progressbar" >>= invoke "progressbar" ()  :: JS t ()
    () <- jQuery ".progressbar" >>= invoke "progressbar" ( "option" :: JSString
                                                   , "max" :: JSString
                                                   , js casesPerTest :: JSNumber
                                                   )
    () <- jQuery ".progressbar" >>= invoke "progressbar" ( "value" :: JSString
                                                   , 0 :: JSNumber
                                                   )
    return ()


  result <- (case teStyle doc of
                TestInPar n -> \ xs -> withPool n $ \ pool -> parallelInterleaved pool xs
                _ -> sequence) $ concat [
      [ do let runTest :: Test -> IO (Result,Timings Double)
               runTest (Test count name test) = do
                 resetTimings (srEngine doc)
                 putStrLn name
                 r <- quickCheckWithResult (stdArgs {chatty=False,maxSuccess=count})
                   $ within (teTimeout doc)
                   $ (if teShrink doc then id else noShrinking)
                   $ callback (afterTestCallback count)
                   $ test
                 t <- getTimings (srEngine doc)
                 print "DONE TESTS IN SR"
                 return (r,fmap realToFrac t)
               execTest :: Test -> IO (Maybe (Timings Double))
               execTest t@(Test _ name _) = do
--                 progressMsg doc name
                 result <- E.try (runTest t >>= E.evaluate)
                 case result of
                   Left (e ::  E.SomeException) -> do
                     print ("EXCEPTION:",e)
                     overwriteMessage doc i j ("Exception!") "failure"
                     E.throw e
                   Right (Success _ _ out,t) -> do
                     putStrLn out
                     overwriteMessage doc i j ("Passed") "success"
                     writeTimings doc i j t
                     return $ Just t
                   Right (GaveUp _ _ out,_) -> do
                     putStrLn out
                     overwriteMessage doc i j ("Gave up") "failure"
                     return Nothing
                   Right (f@Failure {},_) -> do
--                     putStrLn (output f)
--                     putStrLn (reason f)
                     putStrLn $ "FAILED TEST: " ++ name
                     overwriteMessage doc i j ("Failed: " ++ reason f) "failure"
                     -- carry on, please
                     return Nothing     -- failure
                   Right (NoExpectedFailure _ _ out,_) -> do
                     putStrLn out
                     overwriteMessage doc i j ("Ho expected failure") "failure"
                     return Nothing
               afterTestCallback :: Int -> Callback
               afterTestCallback count = PostTest NotCounterexample $ \ state result -> do
                 if not (abort result) && isJust (ok result)
                   then do
                     progressVal doc i j (((numSuccessTests state + 1) * 100) `div` count)
                     if numSuccessTests state `mod` (casesPerTest `div` 10) == 0
                       then do
                         putStr "."
                         hFlush stdout
                       else return ()
                   else do
                     return ()
           execTest t

      | (j::Int,t@(Test _ msg _)) <- [1..] `zip` tests
      ]
    | (i::Int,(txt,tests)) <- [1..] `zip` all_tests
    ]

  async (srEngine doc) $ do
    p <- pbObject 0 0 $ \ n -> "." ++ n ++ " td.progress"
    p # JQuery.html$ js $ "<b align=\"center\">" ++
                    show (length result) ++ " test(s), "++
                    show (length [ () | Just _ <- result ]) ++ " passed / " ++
                    show (length [ () | Nothing <- result ]) ++ " failed " ++
                    "</b>"
    return ()

  let ts :: [Timings [Double]] = [ fmap (:[]) t | Just t <- result ]
  case teStyle doc of
    TestWithTiming | length ts /= 0 -> do
            writeTimings doc 0 0
                $ fmap geometricMean
                $ foldr1 (<>) ts
    _ -> return ()

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


overwriteMessage :: TestEngine -> Int -> Int -> String -> String -> IO ()
overwriteMessage doc i j msg cls = async (srEngine doc) $ do
  p <- pbObject i j $ \ n -> "." ++ n ++ " td.progress"
  p # JQuery.html(js msg)
  p # JQuery.addClass(js cls)
  return ()

writeTimings :: TestEngine -> Int -> Int -> Timings Double -> IO ()
writeTimings doc i j t | teStyle doc /= TestWithTiming = return ()
writeTimings doc i j t = async (srEngine doc) $ do
        pnt 1 (compileTime t)
        pnt 2 (sendTime t)
        pnt 3 (waitTime t)
        return ()
  where
        pnt n v = do
                p <- pbObject i j $ \ nd -> "." ++ nd ++ " td:eq(" ++ show n ++ ")"
                p # JQuery.html (js $ Numeric.showFFloat (Just 2) v "s")

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

-- From http://en.wikipedia.org/wiki/Geometric_mean
geometricMean :: Floating a => [a] -> a
geometricMean xs = exp ((1 / n) * sum (map log xs))
  where n = fromIntegral (length xs)
