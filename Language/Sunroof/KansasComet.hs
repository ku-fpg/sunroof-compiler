{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Sunroof.KansasComet
  ( sync
  , sync'
  , async
  , rsync
  , wait
  , SunroofValue(..)
  , SunroofEngine(..)
  , jsonToJS
  , defaultServerOpts
  , sunroofServer
  , SunroofServerOptions(..)
  , SunroofApp
  ) where

import Data.Aeson.Types ( Value(..), Object, Array )
import Data.Attoparsec.Number ( Number(..) )
--import Data.Boolean
import Data.List ( intercalate )
--import Data.String ( IsString(..) )
import Data.Text ( Text, unpack )
import Data.Proxy
import Data.Default
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

import Control.Monad.IO.Class ( liftIO )

import Network.Wai.Handler.Warp ( Port, settingsPort )
import Network.Wai.Middleware.Static
import qualified Web.Scotty as SC
import Web.KansasComet
  ( Template(..)
  , extract
  --, register
  , Scope
  , send
  , connect
  , queryGlobal
  , Document
  , Options
  , kCometPlugin
  , docUniqs
  )
import qualified Web.KansasComet as KC

import Language.Sunroof.Compiler ( compileJS' )
import Language.Sunroof.Types

-- | The 'SunroofEngine' provides the verbosity level and 
--   kansas comet document to the 'SunroofApp'.
data SunroofEngine = SunroofEngine
  { cometDocument :: Document
  , engineVerbose :: Int -- 0 == none, 1 == inits, 2 == cmds done, 3 == complete log
  }

-- | The number of uniques allocated for the first try of a compilation. 
compileUniqAlloc :: Uniq
compileUniqAlloc = 32

-- | Log the given message on the given level
sunroofLog :: SunroofEngine -> Int -> String -> IO ()
sunroofLog engine level msg = 
  if (engineVerbose engine >= level)
    then do
      putStr "Sunroof> "
      putStrLn msg
    else return ()

-- | Log the compilation result and return it
compileLog :: SunroofEngine -> (String, String) -> IO (String, String)
compileLog engine (src, retVal) = do
  sequence_ $ fmap (sunroofLog engine 3) $ 
    [ "Compiled:", src, "Compiled Return: ", retVal]
  return (src, retVal)

-- | Compile js using unique variables each time.
compile :: (Sunroof a) => SunroofEngine -> JS a -> IO (String, String)
compile engine jsm = do
  -- Allocate a standard amount of uniq for compilation
  uq <- docUniqs compileUniqAlloc (cometDocument engine)
  -- Compile
  let (src, uq') = compileJS' uq jsm
  -- Check if the allocated amount was sufficient
  if (uq' < uq + compileUniqAlloc)
    -- It was sufficient we are finished
    then compileLog engine src
    -- It wasn't sufficient
    else do
      -- Allocate all that are needed
      newUq <- docUniqs (uq' - uq) (cometDocument engine)
      -- Compile again
      compileLog engine $ fst $ compileJS' newUq jsm

-- | Executes the Javascript in the browser without waiting for a result.
async :: SunroofEngine -> JS () -> IO ()
async engine jsm = do
  (res,_) <- compile engine jsm
  send (cometDocument engine) res  -- send it, and forget it
  return ()

-- | Executes the Javascript in the browser and waits for the result value.
--   The result is given as JSON value.
sync' :: (Sunroof a) => SunroofEngine -> JS a -> IO Value
sync' engine jsm = do
  (src,retVar) <- compile engine jsm
  case retVar of
    -- if there is no return value we just represent it as null.
    -- Like this we have something to wait for.
    -- Right now this corresponds to a ~ ().
    ""  -> queryGlobal (cometDocument engine) (src, "null")
    ret -> queryGlobal (cometDocument engine) (src, ret)

-- | Executes the Javascript in the browser and waits for the result.
--   The returned value is just a reference to the computed value.
rsync :: (Sunroof a) => SunroofEngine -> JS a -> IO a
rsync engine jsm = do
  (src, retVar) <- compile engine jsm
  case retVar of
    "" -> do
      error "rsync: Javascript does not have a return value."
    ret -> do
      -- No synchronous call, because this might evaluate
      -- to something that is not representable as JSON.
      -- Also like this we save the bandwidth for transporting
      -- back the value.
      send (cometDocument engine) $ src -- send it, and forget it
      return $ box $ Lit ret

-- | Executes the Javascript in the browser and waits for the result value.
--   The result value is given the corresponding Haskell type,
--   if possible (see 'SunroofValue').
sync :: forall a. (SunroofResult a) => SunroofEngine -> JS a -> IO (ResultOf a)
sync engine jsm = do
  value <- sync' engine jsm
  return $ jsonToValue (Proxy :: Proxy a) value

-- | wait passes an event to a continuation, once. You need
-- to re-register each time.
wait :: Scope -> Template event -> (JSObject -> JS ()) -> JS ()
wait scope tmpl k = do
        o <- function k
        apply (call "$.kc.waitFor") ( string scope
                                    , object (show (map fst (extract tmpl)))
                                    , o
                                    )

-- -----------------------------------------------------------------------
-- Default Server Instance
-- -----------------------------------------------------------------------

-- | A comet application takes the engine/document we are currently communicating 
--   with and delivers the IO action to be executed as server application.
type SunroofApp = SunroofEngine -> IO ()

-- | The 'SunroofServerOptions' specify the configuration of the 
--   sunroof comet server infrastructure.
--   
--   [@cometPort@] The port the server is reachable from.
--   [@cometResourceBaseDir@] Will be used as base directory to 
--     search for the @css@ and @js@ folders which will be forwarded.
--   [@cometIndexFile@] The file to be used as index file.
--   [@cometOptions@] Provides the kansas comet options to use. 
--     Default options are provided with the 'defaultServerOpts'.
--   [@sunroofVerbose@] @0@ for none, @1@ for initializations, 
--     @2@ for commands done and @3@ for a complete log.
--   
--   See 'sunroofCometServer' and 'defaultServerOpts' for further information.
data SunroofServerOptions = SunroofServerOptions
  { cometPort :: Port
  , cometResourceBaseDir :: FilePath
  , cometIndexFile :: FilePath
  , cometOptions :: Options
  , sunroofVerbose :: Int -- 0 == none, 1 == inits, 2 == cmds done, 3 == complete log
  }

-- | Sets up a comet server ready to use with sunroof.
--   
--   @sunroofCometServer opts app@: 
--   The @opts@ give various configuration for the comet server.
--   See 'SunroofServerOptions' for further information on this.
--   The application to run is given by @app@. It takes the current 
--   engine/document as parameter. The document is needed for calls to 'sync',
--   'async' and 'rsync'.
--   
--   The server provides the kansas comet Javascript on the path 
--   @js/kansas-comet.js@.
--   
--   For the index file to setup the communication correctly with the comet
--   server it has to load the @kansas-comet.js@ inside the @head@:
--   
-- >   <script type="text/javascript" src="js/kansas-comet.js"></script>
--   
--   It also has to execute the following Javascript at the end of the
--   index file to initialize the communication:
--   
-- >   <script type="text/javascript">
-- >     $(document).ready(function() {
-- >       $.kc.connect("/ajax");
-- >     });
-- >   </script>
--   
--   The string @/ajax@ has to be set to whatever the comet prefix 
--   in the 'Options' provided by the 'SunroofServerOptions' is. 
--   These snippits will work for the 'defaultServerOpts'.
--   
--   Additional debug information can be displayed in the browser when
--   adding the following element to the index file:
--   
-- >   <div id="debug-log"></div>
--   
--   Look into the example folder to see all of this in action.
sunroofServer :: SunroofServerOptions -> SunroofApp -> IO ()
sunroofServer opts cometApp = do
  let warpSettings = (SC.settings def) { settingsPort = cometPort opts }
  -- Be quiet scotty! ... and beam me up!
  let scottyOptions = def { SC.verbose = 0
                          , SC.settings = warpSettings }
  SC.scottyOpts scottyOptions $ do
    kcomet <- liftIO kCometPlugin
    let pol = only [("", cometIndexFile opts)
                   ,("js/kansas-comet.js", kcomet)]
              <|> ((hasPrefix "css/" <|> hasPrefix "js/") 
                   >-> addBase (cometResourceBaseDir opts))
    SC.middleware $ staticPolicy pol
    connect (cometOptions opts) $ wrapDocument opts cometApp

-- | Wrap the document into the sunroof engine.
wrapDocument :: SunroofServerOptions -> SunroofApp -> (Document -> IO ())
wrapDocument opts cometApp doc = cometApp 
                               $ SunroofEngine 
                               { cometDocument = doc
                               , engineVerbose = sunroofVerbose opts
                               }

-- | Default options to use for the sunroof comet server.
--   
--   [@cometPort@] Default port is @3000@.
--   [@cometResourceBaseDir@] Default resource location is @"."@.
--   [@cometIndexFile@] Default index file is @"index.html"@.
--   [@cometOptions@] Uses the server path @/ajax@ for the comet JSON communication.
--                    Sets verbosity to @0@ (quiet).
--   [@sunroofVerbose@] Is set to @0@ (quiet).
--   
defaultServerOpts :: SunroofServerOptions
defaultServerOpts = SunroofServerOptions
  { cometPort = 3000
  , cometResourceBaseDir = "."
  , cometIndexFile = "index.html"
  , cometOptions = def { KC.prefix = "/ajax", KC.verbose = 0 }
  , sunroofVerbose = 0
  }

-- -----------------------------------------------------------------------
-- JSON Value to Haskell/Sunroof conversion
-- -----------------------------------------------------------------------

-- | Provides correspondant Haskell types for certain Sunroof types.
class (Sunroof a) => SunroofResult a where
  type ResultOf a
  jsonToValue :: Proxy a -> Value -> ResultOf a
  --toJS :: ValueOf a -> a

instance SunroofResult () where
  type ResultOf () = ()
  jsonToValue _ (Null) = ()
  jsonToValue _ _ = error "jsonToValue: JSON value is not unit."
  --toJS () = ()

instance SunroofResult JSBool where
  type ResultOf JSBool = Bool
  jsonToValue _ (Bool b) = b
  jsonToValue _ _ = error "jsonToValue: JSON value is not a boolean."
  --toJS True = true
  --toJS False = false

instance SunroofResult JSNumber where
  type ResultOf JSNumber = Double
  jsonToValue _ (Number (I i)) = fromInteger i
  jsonToValue _ (Number (D d)) = d
  jsonToValue _ _ = error "jsonToValue: JSON value is not a number."
  --toJS = JSNumber . Lit . show

instance SunroofResult JSString where
  type ResultOf JSString = String
  jsonToValue _ (String s) = unpack s
  jsonToValue _ _ = error "jsonToValue: JSON value is not a string."
  --toJS = fromString

-- | Converts a JSON value to a Sunroof Javascript expression.
jsonToJS :: Value -> Expr
jsonToJS (Bool b)       = unbox $ js b
jsonToJS (Number (I i)) = unbox $ js i
jsonToJS (Number (D d)) = unbox $ js d
jsonToJS (String s)     = unbox $ js s
-- TODO: This is only a hack. Could null be a good reprensentation for unit '()'?
jsonToJS (Null)         = Lit "null"
jsonToJS (Array arr)    = jsonArrayToJS arr
jsonToJS (Object obj)   = jsonObjectToJS obj

-- TODO: Some day find a Sunroof representation of this.
jsonObjectToJS :: Object -> Expr
jsonObjectToJS obj = Lit $
  let literalMap = M.toList $ fmap (show . jsonToJS) obj
      convertKey k = "\"" ++ unpack k ++ "\""
      keyValues = fmap (\(k,v) -> convertKey k ++ ":" ++ v) literalMap
  in "{" ++ intercalate "," keyValues ++ "}"

-- TODO: Some day find a Sunroof representation of this.
jsonArrayToJS :: Array -> Expr
jsonArrayToJS arr = Lit $
  "(new Array(" ++ (intercalate "," $ V.toList $ fmap (show . jsonToJS) arr) ++ "))"

instance SunroofValue Value where
  type ValueOf Value = JSObject
  js = js . jsonToJS

instance SunroofValue Text where
  type ValueOf Text = JSString
  js = js . unpack
