{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, KindSignatures, GADTs #-}

-- Example of using Kansas Comet

module Main where

import Data.Aeson as A
import Data.Aeson.Types as AP
import qualified Web.Scotty as Scotty
import Web.Scotty (scottyOpts, get, file, literal, middleware)
import Web.KansasComet as KC
import Data.Default
import Data.Map (Map)
import Control.Monad
--import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Data.Monoid
import Data.List as L
import Control.Monad.IO.Class
import Network.Wai.Middleware.Static
-- import Network.Wai      -- TMP for debug

import qualified Data.Text.Lazy as LT
import qualified Data.Text      as T

main = do
        -- build the scotty dispatch app
        scottyOpts (def { Scotty.verbose = 0 })  $ do
                -- provide some static pages, include jquery
                -- This is scotty code

                let hasPrefix pre x = if pre`isPrefixOf` x then return x else Nothing

                let policy =  only [("","index.html")]
                          <|> ((hasPrefix "css/" <|> hasPrefix "js/") >-> addBase ".")

                middleware $ staticPolicy $ policy

                kcomet <- liftIO kCometPlugin
                get "/js/kansas-comet.js" $ file $ kcomet
                -- connect /example to the following web_app
                connect opts web_app

opts :: KC.Options
opts = def { prefix = "/example", verbose = 0 }

-- This is run each time the page is first accessed
web_app :: Document -> IO ()
web_app doc = do
        print "web_app"

        registerEvents doc (slide <> click)

        let control model = do
                Just res <- waitForEvent doc (slide <> click)
                case res of
                  Slide _ n                      -> view n
                  Click "up"    _ _ | model < 25 -> view (model + 1)
                  Click "down"  _ _ | model > 0  -> view (model - 1)
                  Click "reset" _ _              -> view 0
                  _ -> control model

            view model = do
                let n = model
                send doc $ concat
                        [ "$('#slider').slider('value'," ++ show n ++ ");"
                        , "$('#fib-out').html('fib " ++ show n ++ " = " ++ "&#171;&#8226;&#187;')"
                        ]
                -- sent a 2nd packet, because it will take time to compute fib
                send doc ("$('#fib-out').text('fib " ++ show n ++ " = " ++ show (fib n) ++ "')")

                control model


        forkIO $ control 0

        return ()

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

data Event = Slide String Int
           | Click String Int Int
    deriving (Show)


slide = event "slide" Slide
            <&> "id"      := "$(widget).attr('id')"
            <&> "count"   := "aux.value"

click = event "click" Click
            <&> "id"      := "$(widget).attr('id')"
            <&> "pageX"   :=  "event.pageX"
            <&> "pageY"   :=  "event.pageY"

events = slide <> click

