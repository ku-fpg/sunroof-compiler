{-# LANGUAGE OverloadedStrings #-}
module Web.Tractor where

import Web.Scotty
import Data.Aeson
import Control.Monad
import Language.JavaScript.Parser as JS
import Language.JavaScript.Pretty.Printer as PP
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar as STM
import Control.Monad.IO.Class
import Paths_tractor
import qualified Data.Map as Map
import Control.Concurrent

import qualified Data.Text.Lazy as T

-- | connect "/foobar" (...) gives

-- >  POST http://.../foobar/                     <- bootstrap the interaction
-- >  GET  http://.../foobar/act/<id#>/<act#>     <- get a specific action
-- >  POST http://.../foobar/event/<event-name>   <- send an event as a JSON object

connect :: String              -- ^ URL path prefix for this page
        -> (Document -> IO ()) -- ^ called for access of the page
        -> ScottyM ()
connect prefix callback = do

   -- A unique number generator, or ephemeral generator.
   -- This is the (open) secret between the client and server.
   -- (Why are we using an MVar vs a TMVar? No specific reason here)
   uVar <- liftIO $ newMVar 0
   let getUniq :: IO Int
       getUniq = do
              u <- takeMVar uVar
              putMVar uVar (u + 1)
              return u

   contextDB <- liftIO $ atomically $ newTVar $ (Map.empty :: Map.Map Int Document)
   let newContext :: IO Int
       newContext = do
            uq <- getUniq
            picture <- atomically $ newEmptyTMVar
            callbacks <- atomically $ newTMVar $ Map.empty
            let cxt = Document picture callbacks uq
            liftIO $ atomically $ do
                    db <- readTVar contextDB
                    -- assumes the getUniq is actually unique
                    writeTVar contextDB $ Map.insert uq cxt db
            -- Here is where we actually spawn the user code
            _ <- forkIO $ callback cxt
            return uq

   -- POST starts things off.
   post (capture $ prefix ++ "/") $ do
            uq  <- liftIO $ newContext
            text (T.pack $ "tractor_session = " ++ show uq ++ ";tractor_redraw();")

   -- GET the updates to the documents (should this be an (empty) POST?)
   get (capture $ prefix ++ "/act/:id/:act") $ do
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            num <- param "id"
--            liftIO $ print (num :: Int)

            let tryPushAction :: TMVar JS.JSNode -> ActionM ()
                tryPushAction var = do
                    -- The PUSH archtecture means that we wait upto a second if there
                    -- is not javascript to push yet. This stops a busy-waiting
                    -- (or technically restricts it to once a second)
                    ping <- liftIO $ registerDelay (1000 * 1000)
                    res <- liftIO $ atomically $ do
                            b <- readTVar ping
                            if b then return Nothing else do
                                 liftM Just (takeTMVar var)

                    case res of
                     Just js -> do
                            liftIO $ putStrLn $ PP.renderToString js
                            -- TODO: optimize this through a better API
                            text $ T.pack $ PP.renderToString js
                     Nothing  ->
                            -- give the browser something to do (approx every second)
                            text (T.pack "")

            db <- liftIO $ atomically $ readTVar contextDB
            case Map.lookup num db of
               Nothing  -> text (T.pack $ "alert('Can not find act #" ++ show num ++ "');")
               Just doc -> tryPushAction (sending doc)


   return ()

-- 'jTractorStatic' provide access to the jTractor jQuery plugin. The argument
-- is the path to the collection that holds the static javascript files.
jTractorStatic :: String -> ScottyM ()
jTractorStatic prefix = do
        dataDir <- liftIO getDataDir
        get (literal $ prefix ++ "/jquery-tractor.js") $ file $ dataDir ++ "/static/jquery-tractor.js"

-- 'send' sends a javascript fragement to a document.
-- 'send' suspends the thread until the javascript has been dispatched
-- the the browser.
send :: Document -> JS.JSNode -> IO ()
send doc js = atomically $ putTMVar (sending doc) js

-- | listen sets up a RESTful listener and a new Channel that listens
-- to this listener. It is important to realize that if you call listen twice,
-- you get a duplicate channel.
listen :: (FromJSON a) => Document -> EventName -> IO (TChan a)
listen doc eventName = return undefined

type EventName = String

data Document = Document
        { sending   :: TMVar JS.JSNode          -- ^ Code to be sent to the browser
        , listening :: TMVar (Map.Map EventName (TChan Value))
        , secret    :: Int                      -- ^ the number of this document
        }



