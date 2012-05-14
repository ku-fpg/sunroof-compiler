{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- Example of using tractor

module Main where

import Data.Aeson as A
import Data.Aeson.Types as AP
import Web.Scotty
import Web.Tractor as T
import Data.Default
import Control.Monad
import qualified Control.Applicative as App
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as Text

import Web.Sunroof

main = main_sunroof

main_tractor = do

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

        register doc "click" $ Text.pack $ concat
                [ " return { pageX : event.pageX"
                , "        , pageY : event.pageY"
                , "        , id    : $(widget).attr('id')"
                , "        };"
                ]

        forkIO $ forever $ do
                res <- waitFor doc "click"
                res <- query doc (Text.pack "return { wrapped : $('#fib-in').attr('value') };")
                let Success (Wrapped a) :: Result (Wrapped String) = parse parseJSON res
                print a
                case reads a of
                  [(v :: Int,"")] -> do
                        send doc (Text.pack $ "$('#fib-out').html('&#171;&#8226;&#187;')")
                        send doc (Text.pack $ "$('#fib-out').text('" ++ show (fib v) ++ "')")
                  _ ->  send doc (Text.pack $ "$('#fib-out').text('...')")

--                let Success b :: Result String = parse parseJSON a
--                print b
                print res
        return ()

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

data Wrapped a = Wrapped a
        deriving Show

instance FromJSON a => FromJSON (Wrapped a) where
   parseJSON (Object v) = Wrapped    App.<$>
                          (v .: "wrapped")
   parseJSON _          = mzero


main_sunroof = do

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
                connect opts sunroof_app

sunroof_app :: Document -> IO ()
sunroof_app doc = do
        print "sunroof_app"

        send doc "alert('x');"
        sendS doc $ do
                alert "ABC"
                alert "CDE"

sendS :: (Sunroof a) => Document -> JSM a -> IO ()
sendS doc jsm = do
        let ((txt,ty),_) = runCompM (compileC jsm) 0
        print ("TXT:",txt)
        print ("TY:",ty)
        send doc (Text.pack $ "(" ++ txt ++ ")(function(k){})")
        return ()

