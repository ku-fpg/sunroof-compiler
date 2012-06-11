{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, KindSignatures, GADTs, DoRec #-}

-- Example of using Kansas Comet

module Main where

import Data.Aeson as A
import Data.Aeson.Types as AP
import qualified Web.Scotty as Scotty
import Web.Scotty (scottyOpts, get, file, literal, middleware)
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
import Data.Boolean

-- import Network.Wai      -- TMP for debug

import qualified Web.KansasComet as KC
import Web.KansasComet as KC ((<&>), event, send, registerEvents, waitForEvent, Document, prefix, kCometPlugin, connect, verbose)

import Language.Sunroof
import Language.Sunroof.Compiler
import Language.Sunroof.Types

import qualified Data.Text.Lazy as LT
import qualified Data.Text      as T

-- Async requests that something be done, without waiting for any reply
async :: Document -> JS () -> IO ()
async = undefined

-- Sync requests that something be done, *and* waits for a reply.
sync :: (Sunroof a) => Document -> JS a -> IO a
sync doc jsm = do
        let (res,ret) = compileJS jsm
        print (res,ret)
        send doc $ res
        return $ undefined


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

        sync doc $ do
                obj <- new

                -- This is using the imperative update to enable the
                let control :: JSNumber -> JS ()
                    control m = obj ! "control" <$> with [cast m]

                    view :: JSNumber -> JS ()
                    view m = obj ! "view" <$> with [cast m]

                    set :: (Sunroof a, Sunroof b) => String -> (a -> JS b) -> JS ()
                    set nm f = do n <- function f
                                  obj <$> nm := n

                set "control" $ \ (model :: JSNumber) -> wait (slide <> click) $ \ event -> do
                        model' <- ifB ((event ! "eventname" :: JSString) ==* "slide")
                                        (return (event ! "value" :: JSNumber))
                                $ ifB ((event ! "id" :: JSString) ==* "up")
                                        (return (model + 1))
                                $ ifB ((event ! "id" :: JSString) ==* "down")
                                        (return (model - 1))
                                $ ifB ((event ! "id" :: JSString) ==* "reset")
                                        (return 0)
                                $ (return model)
                        view model'

                set "view" $ \ (model :: JSNumber) -> do
                        () <- call "$('#slider').slider" <$> with [cast ("value" :: JSString), cast model]
                        () <- call "$('#fib-out').html" <$> with [cast $ ("fib " :: JSString) <> cast model]
                        control model

                -- call control
                control 0


{-


                        nm <- eval (event ! "eventname" :: JSString)
                        ifB  (nm ==* "click")
                             (do alert "CLICKy")
                             (do alert "NO CLICKy")
                        alert ("Finally")
--                        alert ("eventname = " <> (ifB (nm ==* "click") "CLICK" "NO CLICK"))
-}

{-

--              foo <$> "c" <$> "d" := c

--                let v = c ! "pageX" :: JSNumber
                v <- eval (c ! "pageX" :: JSNumber)
                nm <- eval (c ! "eventname" :: JSString)
                alert ("you clicked" <> cast v)
                alert ("you clicked" <> cast c)
-}
{-
                switch  [ (nm .==. "click", ...)
                        ]
-}


{-
--        jsSelect $ JSS_Call "foo1" [cast (1 :: JSNumber)] :: JS ()

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
-}
        return ()

fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

data Event = Slide String Int
           | Click String Int Int
    deriving (Show)

(~=) = (KC.:=)

slide = event "slide" Slide
            <&> "id"      ~= "$(widget).attr('id')"
            <&> "value"   ~= "aux.value"

click = event "click" Click
            <&> "id"      ~= "$(widget).attr('id')"
            <&> "pageX"   ~=  "event.pageX"
            <&> "pageY"   ~=  "event.pageY"

events = slide <> click

