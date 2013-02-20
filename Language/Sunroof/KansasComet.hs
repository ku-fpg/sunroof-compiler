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
  , defaultCometIndexFile
  , defaultCometOpts
  , defaultCometPort
  , defaultCometServer
  , sunroofCometServer
  ) where

import Data.Aeson.Types ( Value(..), Object, Array )
import Data.Attoparsec.Number ( Number(..) )
import Data.Boolean
import Data.List ( intercalate )
import Data.String ( IsString(..) )
import Data.Text ( unpack )
import Data.Proxy
import Data.Default
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

import Control.Monad.IO.Class ( liftIO )

import Network.Wai.Handler.Warp ( Port )
import Network.Wai.Middleware.Static
import Web.Scotty (scotty, middleware)
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

data SunroofEngine = SunroofEngine
  { cometDocument :: Document
  , verbose :: Int -- 0 == none, 1 == inits, 2 == cmds done, 3 == complete log
  }

-- | The number of uniques allocated for the first try of a compilation. 
compileUniqAlloc :: Uniq
compileUniqAlloc = 32

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
    then return src
    -- It wasn't sufficient
    else do
      -- Allocate all that are needed
      newUq <- docUniqs (uq' - uq) (cometDocument engine)
      -- Compile again
      return $ fst $ compileJS' newUq jsm

-- | Executes the Javascript in the browser without waiting for a result.
async :: SunroofEngine -> JS () -> IO ()
async engine jsm = do
  (res,_) <- compile engine jsm
  --print res
  send (cometDocument engine) res  -- send it, and forget it
  return ()

-- | Executes the Javascript in the browser and waits for the result value.
--   The result is given as JSON value.
sync' :: (Sunroof a) => SunroofEngine -> JS a -> IO Value
sync' engine jsm = do
  (src,retVar) <- compile engine jsm
  print (src,retVar)
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
sync :: forall a. (SunroofValue a) => SunroofEngine -> JS a -> IO (ValueOf a)
sync engine jsm = do
  value <- sync' engine jsm
  return $ jsonToValue (Proxy :: Proxy a) value

-- This can be build out of primitives
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

-- | A comet application takes the document we are currently communicating 
--   with and delivers the IO action to be executed as server application.
type CometApp = SunroofEngine -> IO ()

-- | Sets up a comet server ready to use with sunroof.
--   
--   @defaultCometServer res app@:
--   Use @res@ as base directory to search for the @css@ and @js@
--   folders which will be forwarded.
--   The application to run is given by @app@. It takes the current 
--   document as parameter. The document is needed for calls to 'sync',
--   'async' and 'rsync'.
--   
--   All other option from 'sunroofCometServer' are supplied 
--   with the default values: 'defaultCometPort', 'defaultCometIndexFile'
--   and 'defaultCometOpts'.
--   
--   See 'sunroofCometServer' for further information.
defaultCometServer :: FilePath -> CometApp -> IO ()
defaultCometServer resBaseDir cometApp = 
  sunroofCometServer defaultCometPort 
                     resBaseDir 
                     defaultCometIndexFile
                     defaultCometOpts
                     cometApp

-- | Sets up a comet server ready to use with sunroof.
--   
--   @sunroofCometServer p res idx opts app@: 
--   Starts a comet server at port @p@. 
--   It will use @res@ as base directory to search for the @css@ and @js@
--   folders which will be forwarded.
--   The @idx@ file will be used as index file.
--   @opts@ provides the kansas comet options to use. Default options
--   are provided with 'defaultCometOpts'.
--   The application to run is given by @app@. It takes the current 
--   document as parameter. The document is needed for calls to 'sync',
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
--   in the given 'Options' is. These snippits will work for 'defaultCometOpts'.
--   
--   Additional debug information can be displayed in the browser when
--   adding the following element to the index file:
--   
-- >   <div id="debug-log"></div>
--   
--   Look into the example folder to see all of this in action.
sunroofCometServer :: Port -> FilePath -> FilePath -> Options -> CometApp -> IO ()
sunroofCometServer port resBaseDir indexFile options cometApp = 
  scotty port $ do
    kcomet <- liftIO kCometPlugin
    let pol = only [("",indexFile)
                   ,("js/kansas-comet.js", kcomet)]
              <|> ((hasPrefix "css/" <|> hasPrefix "js/") >-> addBase resBaseDir)
    middleware $ staticPolicy pol
    connect options $ wrapDocument cometApp

wrapDocument :: CometApp -> (Document -> IO ())
wrapDocument cometApp doc = cometApp 
                          $ SunroofEngine 
                          { cometDocument = doc
                          , verbose = 0
                          }

-- | Default options to use for the comet server.
--   Uses the server path @/ajax@ for the comet JSON communication.
--   Sets verbosity to 0 (quiet).
defaultCometOpts :: Options
defaultCometOpts = def { KC.prefix = "/ajax", KC.verbose = 0 }

-- | The default port is @3000@.
defaultCometPort :: Port
defaultCometPort = 3000

-- | The default index file is @index.html@
defaultCometIndexFile :: FilePath
defaultCometIndexFile = "index.html"

-- -----------------------------------------------------------------------
-- JSON Value to Haskell/Sunroof conversion
-- -----------------------------------------------------------------------

-- | Provides correspondant Haskell types for certain Sunroof types.
class (Sunroof a) => SunroofValue a where
  type ValueOf a
  jsonToValue :: Proxy a -> Value -> ValueOf a
  --toJS :: ValueOf a -> a

instance SunroofValue () where
  type ValueOf () = ()
  jsonToValue _ (Null) = ()
  jsonToValue _ _ = error "jsonToValue: JSON value is not unit."
  --toJS () = ()

instance SunroofValue JSBool where
  type ValueOf JSBool = Bool
  jsonToValue _ (Bool b) = b
  jsonToValue _ _ = error "jsonToValue: JSON value is not a boolean."
  --toJS True = true
  --toJS False = false

instance SunroofValue JSNumber where
  type ValueOf JSNumber = Double
  jsonToValue _ (Number (I i)) = fromInteger i
  jsonToValue _ (Number (D d)) = d
  jsonToValue _ _ = error "jsonToValue: JSON value is not a number."
  --toJS = JSNumber . Lit . show

instance SunroofValue JSString where
  type ValueOf JSString = String
  jsonToValue _ (String s) = unpack s
  jsonToValue _ _ = error "jsonToValue: JSON value is not a string."
  --toJS = fromString

-- | Converts a JSON value to a Sunroof Javascript expression.
jsonToJS :: Value -> Expr
jsonToJS (Bool b)       = unbox (if b then false else true :: JSBool)
jsonToJS (Number (I i)) = unbox (fromInteger i :: JSNumber)
jsonToJS (Number (D d)) = unbox (fromRational (toRational d) :: JSNumber)
jsonToJS (String s)     = unbox (fromString (unpack s) :: JSString)
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

