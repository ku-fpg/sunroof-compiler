{-# LANGUAGE OverloadedStrings, TypeFamilies, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Prelude hiding (mod, div)

import Data.Monoid
import Data.Boolean
import Data.Boolean.Numbers hiding (floor, round)
import Data.Default
import Data.List
import Data.Char ( isControl, isAscii )

import Control.Concurrent

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class

import Network.Wai.Middleware.Static
import Web.Scotty (scotty, middleware)
import Web.KansasComet
import qualified Web.KansasComet as KC

import Language.Sunroof
import Language.Sunroof.JS.JQuery (jQuery)

import System.Random

import Data.Ratio

import Test.QuickCheck hiding ( assert )
import Test.QuickCheck.Monadic ( monadicIO, assert, run, pick, pre )
import Test.QuickCheck.Gen ( Gen(MkGen, unGen) )

main :: IO ()
main = sunroofServer (defaultServerOpts { cometResourceBaseDir = ".." }) web_app

default(JSNumber, JSString, String)

type instance BooleanOf () = JSBool

-- This is run each time the page is first accessed
web_app :: SunroofEngine -> IO ()
web_app doc = do
        -- We use the lower-level waitForEvent, so we can test the JS compiler.
        forkIO $ do
                print "waiting"
                a <- waitForEvent (cometDocument doc) "session" abort
                print "waited"
                print a
        
        runTests doc
          [ T "Constant Numbers" (checkConstNumber doc :: Double -> Property)
          , T "Constant Unit"    (checkConstValue doc :: () -> Property)
          , T "Constant Boolean" (checkConstValue doc :: Bool -> Property)
          , T "Constant String"  (checkConstValue doc :: String -> Property)
          , T "Basic Addition"       (checkBasicArith doc (+) :: Double -> Double -> Property)
          , T "Basic Subtraction"    (checkBasicArith doc (-) :: Double -> Double -> Property)
          , T "Basic Multiplication" (checkBasicArith doc (*) :: Double -> Double -> Property)
          , T "Arbitrary Arithmetic" (checkArbitraryArith doc)
          ]
        {-
        let assert True msg = return ()
            assert False msg = error $ "test failed: " ++ msg
        -}
        
        {-
        putStrLn "-- Check constant numbers"
        sequence_
         [ do putStrLn $ "checking : " ++ show n
              n' <- sync doc (return $ js n)
              assert (n == n') $ "expecting " ++ show n ++ ", found " ++ show n'
         | n <- [1..10] ++ [0,-1] :: [Double]
         ]

        putStrLn "-- Check basic arithmetic (a op b)"
        sequence_
         [ do putStrLn $ "checking : " ++ show n ++ " " ++ nm ++ " " ++ show m
              let r = (n `f` m)
              r' <- sync doc (return (jsNumber n `f` jsNumber m :: JSNumber))
              assert (r == r') $ "expecting " ++ show r ++ ", found " ++ show r'
         | Op2 f nm <- op2s
         , n <- [-1..3]
         , m <- [-1..3]
         ]
                 -}
         
        {-
        putStrLn "-- Check basic arithmetic expressions"

        let (s0,s1) = split $ mkStdGen 0

        let xs = unfoldr (return . split) s0

        sequence_
         [ do putStrLn $ "checking : " ++ show (e :: JSNumber)
              let r = e :: Double
              r' :: Double <- sync doc (return (e :: JSNumber))
              assert (r == r') $ "expecting " ++ show r ++ ", found " ++ show r'
         | s <- take 100 xs
         , let e :: Num a => a
               e = unArb (expr 5) s
         , abs e < (100000000 :: Double)
         ]

        putStrLn "-- passed all tests"
        -}

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
  n <- pick $ choose (1, 10)
  (r, e) <- pick $ sameSeed (numExprGen n :: Gen Double) 
                            (numExprGen n :: Gen JSNumber)
  pre $ abs r < (100000000 :: Double)
  r' <- run $ sync doc (return e)
  assert $ r `deltaEqual` r'

-- -----------------------------------------------------------------------
-- Test execution
-- -----------------------------------------------------------------------

data T = forall a. Testable a => T String a

runTests :: SunroofEngine -> [T] -> IO ()
runTests doc tests = do
  let testCount = length tests
  progressMax doc testCount
  progressVal doc 0
  execTests tests
  where
    runTest :: T -> IO Result
    runTest (T name test) = do
      putStrLn name
      quickCheckWithResult (stdArgs {chatty=False}) test
    execTests :: [T] -> IO ()
    execTests [] = putStrLn "PASSED ALL TESTS"
    execTests (t@(T name _):ts) = do
      progressInc doc name
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

progressMax :: SunroofEngine -> Int -> IO ()
progressMax doc n = async doc $ do
  p <- jQuery "#progressbar" 
  p # method "progressbar" ( "option" :: JSString
                           , "max" :: JSString
                           , js n :: JSNumber)

progressVal :: SunroofEngine -> Int -> IO ()
progressVal doc n = async doc $ do
  p <- jQuery "#progressbar" 
  p # method "progressbar" ( "option" :: JSString
                           , "value" :: JSString
                           , js n :: JSNumber)

progressInc :: SunroofEngine -> String -> IO ()
progressInc doc msg = async doc $ do
  p <- jQuery "#progressbar" 
  l <- jQuery "#plabel"
  n <- p # method "progressbar" ( "option" :: JSString
                                , "value" :: JSString)
  p # method "progressbar" ( "option" :: JSString
                           , "value" :: JSString
                           , n + 1 :: JSNumber) :: JS ()
  l # method "text" (js msg :: JSString)

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
data Op2 = Op2 (forall a . Num a => a -> a -> a) String
op2s = [ Op2 (+) "+", Op2 (-) "-", Op2 (*) "*"]

jsNumber :: Double -> JSNumber
jsNumber = fromRational . toRational

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
-}
{-
--arithExpr :: (Num a) => Int -> Int -> a

fromIntegralArb :: (Num b) => Arb  b
fromIntegralArb = fromIntegral

op2 =
  where
   ops :: Num a => Arb (a -> a -> a)
   ops = finite [(+),(-),(*)]
-}
{-
a1 = finite [1]
a2 = finite [2]

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
{-
diag :: Integer -> (Integer,Integer)
diag 0 = (0,0)
diag 1 = (1,0)
diag 2 = (0,1)
diag 3 = (2,0)
diag 4 = (1,1)
diag 5 = (0,2)
diag 6 = (
  -}
