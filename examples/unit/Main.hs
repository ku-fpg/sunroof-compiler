{-# LANGUAGE OverloadedStrings, TypeFamilies, ScopedTypeVariables, RankNTypes #-}

module Main where

import Prelude hiding (mod, div)

import Data.Monoid
import Data.Boolean
import Data.Boolean.Numbers hiding (floor, round)
import Data.Default
import Data.List

import Control.Concurrent

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class

import Network.Wai.Middleware.Static
import Web.Scotty (scotty, middleware)
import Web.KansasComet
import qualified Web.KansasComet as KC

import Language.Sunroof

import System.Random
--import Language.Sunroof.Types
--import Language.Sunroof.Canvas
--import Language.Sunroof.Browser hiding ( eval )

import Data.Ratio

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

        let assert True msg = return ()
            assert False msg = error $ "test failed: " ++ msg

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
              r' <- sync doc (return (js n `f` js m :: JSNumber))
              assert (r == r') $ "expecting " ++ show r ++ ", found " ++ show r'
         | Op2 f nm <- op2s
         , n <- [-1..3]
         , m <- [-1..3]
         ]

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

