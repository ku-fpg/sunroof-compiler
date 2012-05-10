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
import Data.Default

import qualified Data.Text.Lazy as T

-- | connect "/foobar" (...) gives

-- >  POST http://.../foobar/                     <- bootstrap the interaction
-- >  GET  http://.../foobar/act/<id#>/<act#>     <- get a specific action
-- >  POST http://.../foobar/event/<event-name>   <- send an event as a JSON object

connect :: Options             -- ^ URL path prefix for this page
        -> (Document -> IO ()) -- ^ called for access of the page
        -> ScottyM ()
connect opt callback = do
   when (verbose opt >= 1) $ liftIO $ putStrLn $ "tractor connect with prefix=" ++ show (prefix opt)

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
   post (capture $ prefix opt ++ "/") $ do
            liftIO $ print "got root"
            uq  <- liftIO $ newContext
            text (T.pack $ "tractor_session = " ++ show uq ++ ";alert('HelXlo');tractor_redraw(0);")

   -- GET the updates to the documents (should this be an (empty) POST?)

   liftIO $ print $ prefix opt ++ "/act/:id/:act"
   get (capture $ prefix opt ++ "/act/:id/:act") $ do
            liftIO $ print "H!"
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            num <- param "id"

            when (verbose opt >= 2) $ liftIO $ putStrLn $
                "tractor: get .../act/" ++ show num
            liftIO $ print (num :: Int)

            let tryPushAction :: TMVar T.Text -> ActionM ()
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
                            liftIO $ putStrLn $ show js
                            text js
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
jTractorStatic :: IO String
jTractorStatic = do
        dataDir <- getDataDir
        return $ dataDir ++ "/static/jquery-tractor.js"

-- 'send' sends a javascript fragement to a document.
-- 'send' suspends the thread until the javascript has been dispatched
-- the the browser.
send :: Document -> T.Text -> IO ()
send doc js = atomically $ putTMVar (sending doc) js

-- | listen sets up a RESTful listener and a new Channel that listens
-- to this listener. It is important to realize that if you call listen twice,
-- you get a duplicate channel.
listen :: (FromJSON a) => Document -> EventName -> IO (TChan a)
listen doc eventName = return (error "listen")

type EventName = String

data Document = Document
        { sending   :: TMVar T.Text             -- ^ Code to be sent to the browser
        , listening :: TMVar (Map.Map EventName (TChan Value))
        , secret    :: Int                      -- ^ the number of this document
        }

data Options = Options
        { prefix  :: String
        , verbose :: Int                -- 0 == none, 1 == inits, 2 == cmds done, 3 == complete log
        }

instance Default Options where
  def = Options
        { prefix = ""                   -- default to root, this assumes single page, etc.
        , verbose = 1
        }


