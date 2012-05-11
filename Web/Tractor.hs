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
-- >  POST http://.../foobar/event/<id#>/<event-name>   <- send an event as a JSON object

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
            callbacks <- atomically $ newTVar $ Map.empty
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
            text (T.pack $ "tractor_session = " ++ show uq ++ ";tractor_redraw(0);")

   -- GET the updates to the documents (should this be an (empty) POST?)

   liftIO $ print $ prefix opt ++ "/act/:id/:act"
   get (capture $ prefix opt ++ "/act/:id/:act") $ do
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            num <- param "id"

            when (verbose opt >= 2) $ liftIO $ putStrLn $
                "tractor: get .../act/" ++ show num
--            liftIO $ print (num :: Int)

            let tryPushAction :: TMVar T.Text -> ActionM ()
                tryPushAction var = do
                    -- The PUSH archtecture means that we wait upto 3 seconds if there
                    -- is not javascript to push yet. This stops a busy-waiting
                    -- (or technically restricts it to once every 3 second busy)
                    ping <- liftIO $ registerDelay (3 * 1000 * 1000)
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


   post (capture $ prefix opt ++ "/event/:id/:event") $ do
           header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
           num <- param "id"
           event <- param "event"
--           liftIO $ print (num :: Int, event :: String)
           val <- jsonData
--           liftIO $ print (val :: Value)
           db <- liftIO $ atomically $ readTVar contextDB
           case Map.lookup num db of
               Nothing  -> do
                   liftIO $ print ("ignoring event",event,val :: Value)
                   text (T.pack $ "alert('Ignore event for session #" ++ show num ++ "');")
               Just doc -> do
                   liftIO $ do
                         ch <- listen doc event
                         print ("sending",event,val)
                         atomically $ writeTChan ch val
                         print ("sent",event,val)
                   text $ T.pack ""


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
-- you get the *same* channel.
listen :: Document -> EventName -> IO (TChan Value)
listen doc eventName = atomically $ do
        db <- readTVar (listening doc)
        case Map.lookup eventName db of
          Just ch -> return ch
          Nothing -> do
             ch <- newTChan
             writeTVar (listening doc) $ Map.insert eventName ch db
             return ch

-- The Text argument returns an object, which is what the event sends to Haskell.
register :: Document -> EventName -> T.Text -> IO ()
register doc eventName eventBuilder =
        send doc $ T.pack $ concat
                        [ "tractor_register(" ++ show eventName ++ ",function(event,widget) {"
                        , T.unpack eventBuilder
                        , "});"
                        ]

-- TODO: make thread safe
-- The test ends with a return for the value you want to see.
query :: Document -> T.Text -> IO Value
query doc qText = do
        ch <- listen doc "reply"       -- should we do a new chanel for each?
        send doc $ T.pack $ concat
                [ "tractor_reply(function(){"
                , T.unpack qText
                , "}());"
                ]
        print "waiting for query result"
        r <- atomically $ readTChan ch
        print ("got result",r)
        return r



type EventName = String

data Document = Document
        { sending   :: TMVar T.Text             -- ^ Code to be sent to the browser
                                                -- This is a TMVar to stop the generation
                                                -- getting ahead of the rendering engine
        , listening :: TVar (Map.Map EventName (TChan Value))
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


