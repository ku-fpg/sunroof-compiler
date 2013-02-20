{-# LANGUAGE OverloadedStrings, TypeFamilies, ScopedTypeVariables, RankNTypes #-}

module Main where

import Prelude hiding (mod, div)

import Data.Monoid
import Data.Boolean
import Data.Boolean.Numbers hiding (floor, round)
import Data.Default

import Control.Concurrent

import Control.Monad
import Control.Monad.IO.Class

import Network.Wai.Middleware.Static
import Web.Scotty (scotty, middleware)
import Web.KansasComet
import qualified Web.KansasComet as KC

import Language.Sunroof
import Language.Sunroof.Types
import Language.Sunroof.Canvas
import Language.Sunroof.Browser hiding ( eval )

main :: IO ()
main = defaultCometServer ".." web_app

opts :: KC.Options
opts = def { prefix = "/example", verbose = 3 }

default(JSNumber, JSString, String)

type instance BooleanOf () = JSBool

-- This is run each time the page is first accessed
web_app :: SunroofDoc -> IO ()
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
              n' <- sync doc (return (jsNumber n :: JSNumber))
              assert (n == n') $ "expecting " ++ show n ++ ", found " ++ show n'
         | n <- [1..10] ++ [0,-1] :: [Double]
         ]

        putStrLn "-- Check basic arithmetic"
        sequence_
         [ do putStrLn $ "checking : " ++ show n ++ " " ++ nm ++ " " ++ show m
              r' <- sync doc (return (jsNumber n `f` jsNumber m :: JSNumber))
              assert ((n `f` m) == r') $ "expecting " ++ show (n `f` m) ++ ", found " ++ show r'
         | Op2 f nm <- [ Op2 (+) "+"
                       , Op2 (-) "-"
                       , Op2 (*) "*"
                       ]
         , n <- [-1..3]
         , m <- [-1..3]
         ]

        putStrLn "-- passed all tests"

data Op2 = Op2 (forall a . Num a => a -> a -> a) String

jsNumber :: Double -> JSNumber
jsNumber = fromRational . toRational

