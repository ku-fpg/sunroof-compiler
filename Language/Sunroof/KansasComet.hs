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
  , Document, queryGlobal
  , Options(..)
  , kCometPlugin
  )

import Language.Sunroof.Compiler
import Language.Sunroof.Types

-- | Executes the Javascript in the browser without waiting for a result.
async :: Document -> JS () -> IO ()
async doc jsm = do
  let (res,_) = compileJS jsm
  --print res
  send doc $ res  -- send it, and forget it
  return ()

-- | Executes the Javascript in the browser and waits for the result value.
--   The result is given as JSON value.
sync' :: (Sunroof a) => Document -> JS a -> IO Value
sync' doc jsm = do
  let (src,retVar) = compileJS jsm
  --print (res,retVar)
  case retVar of
    -- if there is no return value we just represent it as null.
    -- Like this we have something to wait for.
    -- Right now this corresponds to a ~ ().
    ""  -> queryGlobal doc (src, "null")
    ret -> queryGlobal doc (src, ret)

-- | Executes the Javascript in the browser and waits for the result.
--   The returned value is just a reference to the computed value.
rsync :: (Sunroof a) => Document -> JS a -> IO a
rsync doc jsm = do
  let (src, retVar) = compileJS jsm
  case retVar of
    "" -> do
      error "rsync: Javascript does not have a return value."
    ret -> do
      -- No synchronous call, because this might evaluate 
      -- to something that is not representable as JSON.
      -- Also like this we save the bandwidth for transporting
      -- back the value.
      send doc $ src -- send it, and forget it
      return $ box $ Lit ret

-- | Executes the Javascript in the browser and waits for the result value.
--   The result value is given the corresponding Haskell type,
--   if possible (see 'SunroofValue').
sync :: forall a. (SunroofValue a) => Document -> JS a -> IO (ValueOf a)
sync doc jsm = do
  value <- sync' doc jsm
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
type CometApp = Document -> IO ()

-- | Sets up a comet server ready to use with sunroof.
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
defaultCometServer :: FilePath -> CometApp -> IO ()
defaultCometServer resBaseDir cometApp = 
  sunroofCometServer defaultCometPort 
                     resBaseDir 
                     defaultCometIndexFile
                     defaultCometOpts
                     cometApp

-- | Sets up a comet server ready to use with sunroof.
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
sunroofCometServer :: Port -> FilePath -> FilePath -> Options -> CometApp -> IO ()
sunroofCometServer port resBaseDir indexFile options cometApp = 
  scotty port $ do
    kcomet <- liftIO kCometPlugin
    let pol = only [("",indexFile)
                   ,("js/kansas-comet.js", kcomet)]
              <|> ((hasPrefix "css/" <|> hasPrefix "js/") >-> addBase resBaseDir)
    middleware $ staticPolicy pol
    connect options cometApp

-- | Default options to use for the comet server.
--   Uses the server path @json/@ for the comet JSON communication.
--   Sets verbosity to 0 (quiet).
defaultCometOpts :: Options
defaultCometOpts = def { prefix = "json/", verbose = 0 }

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

