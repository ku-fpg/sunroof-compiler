{-# LANGUAGE OverloadedStrings #-}

-- Example of using tractor

module Main where


import Web.Scotty
import Web.Tractor as T
import Data.Default
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as Text


main = do

        -- build the scotty dispatch app
        scotty 3000 $ do
                -- provide some static pages, include jquery
                -- This is scotty code
                get "/" $ file $ "index.html"
                sequence_ [ get (literal ("/" ++ nm)) $ file $  nm
                          | nm <- ["jquery.js","jquery-json.js"]
                          ]
                j_tractor <- liftIO jTractorStatic
                get "/jquery-tractor.js" $ file $ j_tractor

                -- connect /example to the following web_app
                connect opts web_app

opts :: T.Options
opts = def { prefix = "/example", verbose = 2 }

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = do
        print "web_app"
        send doc "tractor_return({result:99});";

        res <- query doc (Text.pack "return { x : $('#fib-in').attr('value') };")
        print res

{-
        results <- listen doc "result"

        forkIO $ forever $ do
                val <- atomically $ readTChan results
                print val
-}
        return ()

-- $("#fib-in").attr("value")
