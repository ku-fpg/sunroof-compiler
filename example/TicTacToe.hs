{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import qualified Control.Applicative as App
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T

import Network.Wai.Middleware.Static
import Web.Scotty
import Web.Tractor

main = scotty 3000 $ do
    middleware static

    get "/" $ file "tictactoe.html"

    j_tractor <- liftIO jTractorStatic
    get "/jquery-tractor.js" $ file $ j_tractor

    -- connect /example to the following web_app
    connect (def { verbose = 3 }) web_app

web_app :: Document -> IO ()
web_app doc = do
    register doc "click" "return { id : $(widget).attr('id') };"

    turn <- newMVar "x.png"

    forkIO $ forever $ do
        res <- waitFor doc "click"
        let Success (Id i) :: Result Id = parse parseJSON res
        print i

        img <- takeMVar turn
        if img == "x.png" then putMVar turn "o.png" else putMVar turn "x.png"
        send doc $ mconcat ["$('#", i, "').attr('src', '", img,"')"]

    return ()

data Id = Id T.Text deriving Show

instance FromJSON Id where
   parseJSON (Object v) = Id App.<$> T.pack App.<$> (v .: "id")
   parseJSON _          = mzero
