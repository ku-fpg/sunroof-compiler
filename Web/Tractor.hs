module Web.Tractor where

import Web.Scotty
import Data.Aeson
import Language.JavaScript.Parser as JS
import Control.Concurrent.STM as STM
import Control.Concurrent.MVar as STM
import Control.Monad.IO.Class
import Paths_tractor
import qualified Data.Map as Map

--import qualified Data.Text.Lazy as T

-- | connect "/foobar" (...) gives

-- >  GET http://.../foobar/init              <- bootstrap the interaction
-- >  GET http://.../foobar/act/<id#>/<act#>  <- get a specific action
-- >  PUT http://.../foobar/event/<event-name>      <- send an event as a JSON object

connect :: String              -- ^ URL path prefix for this page
        -> (Document -> IO ()) -- ^ called for access of the page
        -> ScottyM ()
connect prefix callback = do

   -- A unique number generator, or ephemeral generator.
   -- This is the (open) secret between the client and server.
   uVar <- liftIO $ newMVar 0
   let getUniq :: IO Int
       getUniq = do
              u <- takeMVar uVar
              putMVar uVar (u + 1)
              return u
{-
   contextDB <- newMVar $ (Map.empty :: Map.Map Int Context)
   let newContext :: (Float,Float) -> IO Int
       newContext (w,h) = do
            uq <- getUniq
            picture <- newEmptyMVar
            callbacks <- newMVar $ Map.empty
            let cxt = Context (w,h) picture callbacks uq
            db <- takeMVar contextDB
            putMVar contextDB $ Map.insert uq cxt db
            -- Here is where we actually spawn the user code
            _ <- forkIO $ actions cxt
            return uq

        post (capture $ prefix ++ "/init") $ do
            req <- jsonData
            uq  <- liftIO $ newContext req
            text (T.pack $ "session = " ++ show uq ++ ";redraw();")
-}
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
-- to this listener.
listen :: (FromJSON a) => Document -> EventName -> IO (TChan a)
listen = undefined

type EventName = String

data Document = Document
        { sending   :: TMVar JS.JSNode
        , listening :: ()
        }



