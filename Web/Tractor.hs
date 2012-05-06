module Web.Tractor where

import Web.Scotty
import Data.Aeson
import Language.JavaScript.Parser as JS
import Control.Concurrent.STM as STM

-- | connect "/foobar/" (...) gives

-- >  GET http://.../foobar/init              <- bootstrap the interaction
-- >  GET http://.../foobar/act/<id#>/<act#>  <- get a specific action
-- >  PUT http://.../foobar/event/<event-name>      <- send an event as a JSON object

connect :: String              -- ^ URL path prefix for this page
        -> (Document -> IO ()) -- ^ called for access of the page
        -> ScottyM ()
connect = undefined

send :: Document -> JS.JSNode -> IO ()
send = undefined

-- | listen sets up a RESTful listener and a new Channel that listens
-- to this listener.
listen :: (FromJSON a) => Document -> EventName -> IO (TChan a)
listen = undefined

type EventName = String

data Document = D

