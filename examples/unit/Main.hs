{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

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

        let assert True = return ()
            assert False = error "test failed: aborting"

        putStrLn "-- Check constant numbers"
        sequence_
         [ do putStrLn $ "checking : " ++ show n
              n' <- sync doc (return (jsNumber n :: JSNumber))
              assert (n == n')
         | n <- [1..10] ++ [0,-1] :: [Double]
         ]

        putStrLn "-- Check arithmetic"
        sequence_
         [ do putStrLn $ "checking : " ++ show n ++ " " ++ nm ++ " " ++ show m
              r' <- sync doc (return (jsNumber n `f` jsNumber m :: JSNumber))
              assert ((n + m) == r')
         | (f,nm) <- [((+),"+"),((-),"-"),((*),"*")]
         , n <- [-1..3]
         , m <- [-1..3]
         ]

        putStrLn "-- passed all tests"

jsNumber :: Double -> JSNumber
jsNumber = box . Lit . show

