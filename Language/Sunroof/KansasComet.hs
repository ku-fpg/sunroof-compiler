
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Sunroof.KansasComet
  -- Basic Comet
  ( syncJS
  , asyncJS
  , SunroofResult(..)
  , SunroofEngine(..)
  , jsonToJS
  , sunroofServer
  , SunroofServerOptions(..)
  , SunroofApp
  , debugSunroofEngine
  -- Uplink
  , Uplink
  , newUplink
  , getUplink
  , putUplink
  -- Downlink
  , Downlink
  , newDownlink
  , getDownlink
  , putDownlink
  -- Timing
  , Timings(..)
  , newTimings
  , resetTimings
  , getTimings
  ) where

import Data.Aeson.Types ( Value(..), Object, Array )
import Data.Attoparsec.Number ( Number(..) )
import Data.List ( intercalate )
import Data.Text ( Text, unpack )
import Data.Proxy ( Proxy(..) )
import Data.Default ( Default(..) )
import Data.Semigroup
import Data.Time.Clock
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent.STM

import Network.Wai.Handler.Warp ( Port, settingsPort )
import Network.Wai.Middleware.Static
  ( only, hasPrefix, addBase, staticPolicy
  , (<|>), (>->) )
import qualified Web.Scotty as SC
import Web.KansasComet
  ( send, connect
  , Document, Options
  , kCometPlugin )
import qualified Web.KansasComet as KC

import Language.Sunroof.Types
  ( T(..), JS
  , nullJS
  , apply, fun, done, callcc )
import Language.Sunroof.JavaScript
  ( Expr, Type(Unit)
  , literal, showExpr
  , scopeForEffect )
import Language.Sunroof.Classes ( Sunroof(..), SunroofValue(..), Uniq )
import Language.Sunroof.Compiler ( compileJSI, extractProgramJS, CompilerOpts(..) )
import Language.Sunroof.JS.Bool ( JSBool )
import Language.Sunroof.JS.Object ( JSObject, object )
import Language.Sunroof.JS.String ( JSString, string )
import Language.Sunroof.JS.Number ( JSNumber )
import Language.Sunroof.JS.Array ( JSArray )

-- -------------------------------------------------------------
-- Communication and Compilation
-- -------------------------------------------------------------

-- | The 'SunroofEngine' provides the verbosity level and
--   kansas comet document to the 'SunroofApp'.
data SunroofEngine = SunroofEngine
  { cometDocument :: Document
  , uVar          :: TVar Int   -- Uniq number supply for our engine
  , engineVerbose :: Int        -- 0 == none, 1 == inits, 2 == cmds done, 3 == complete log
  , compilerOpts  :: CompilerOpts
  , timings       :: Maybe (TVar (Timings NominalDiffTime))
  }

-- TODO: rename these internal functions?
-- Generate one unique integer from the document.
docUniq :: SunroofEngine -> IO Int
docUniq = docUniqs 1

-- Generate n unique integers from the document.
docUniqs :: Int -> SunroofEngine -> IO Int
docUniqs n doc = atomically $ do
        u <- readTVar (uVar doc)
        writeTVar (uVar doc) (u + n)
        return u

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
compileLog :: SunroofEngine -> String -> IO ()
compileLog engine src = do
  sequence_ $ fmap (sunroofLog engine 3) $
    [ "Compiled:", src]
  return ()


-- | Compile js using unique variables each time.
compileRequestJS :: SunroofEngine -> JS t () -> IO String
compileRequestJS engine jsm = do
  -- Allocate a standard amount of uniq for compilation
  uq <- docUniqs compileUniqAlloc engine
  -- Compile
  (stmts, uq') <- compileJSI (compilerOpts engine) uq $ extractProgramJS return jsm
  -- Check if the allocated amount was sufficient
  let txt = showExpr False $ scopeForEffect stmts

  if (uq' < uq + compileUniqAlloc)
    -- It was sufficient we are finished
    then do compileLog engine txt
            return txt
    -- It wasn't sufficient
    else do
      -- Allocate all that are needed
      newUq <- docUniqs (uq' - uq) engine
      -- Compile again
      (stmts', _) <- compileJSI (compilerOpts engine) newUq $ extractProgramJS return jsm
      let txt' = showExpr False $ scopeForEffect stmts'
      compileLog engine txt'
      return txt'

-- | Executes the Javascript in the browser without waiting for a result.
asyncJS :: SunroofEngine -> JS t () -> IO ()
asyncJS engine jsm = do
  src <- compileRequestJS engine jsm
  send (cometDocument engine) src  -- send it, and forget it
  return ()

{-
-- | Executes the Javascript in the browser and waits for the result.
--   The returned value is just a reference to the computed value.
rsync :: (Sunroof a) => SunroofEngine -> JS A a -> IO a
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
-}
-- | Executes the Javascript in the browser and waits for the result value.
--   The result value is given the corresponding Haskell type,
--   if possible (see 'SunroofResult').

syncJS :: forall a t . (SunroofResult a) => SunroofEngine -> JS t a -> IO (ResultOf a)
syncJS engine jsm | typeOf (Proxy :: Proxy a) == Unit = do
  _ <- syncJS engine (jsm >> return (0 :: JSNumber))
  return $ jsonToValue (Proxy :: Proxy a) Null
syncJS engine jsm = do
  up <- newUplink engine
  t0 <- getCurrentTime
  src <- compileRequestJS engine (jsm >>= putUplink up)
  addCompileTime engine t0
  t1 <- getCurrentTime
  send (cometDocument engine) src
  addSendTime engine t1
  t2 <- getCurrentTime
  -- There is *no* race condition in here. If no-one is listening,
  -- then the numbered event gets queued up.
  r <- getUplink up
  addWaitTime engine t2
  return r

-- | wait passes an event to a continuation, once. You need
-- to re-register each time.


--up :: Program (JSI t) () -> JS B ()
--up = undefined

--  ((a -> Program (JSI t) ()) -> Program (JSI t) ())
{-
wait :: Scope -> Template event -> JS B JSObject
wait scope tmpl = callcc $ \ o -> do
  () <- apply (fun "$.kc.waitFor") ( string scope
                                   , object (show (map fst (extract tmpl)))
                                   , o
                                   )
  done
-}

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
--
--   [@cometResourceBaseDir@] Will be used as base directory to
--     search for the @css@ and @js@ folders which will be forwarded.
--
--   [@cometIndexFile@] The file to be used as index file.
--
--   [@cometOptions@] Provides the kansas comet options to use.
--     Default options are provided with the 'defaultServerOpts'.
--
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
  , sunroofCompilerOpts :: CompilerOpts
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
wrapDocument opts cometApp doc = do
        uqVar <- atomically $ newTVar 0
        cometApp $ SunroofEngine
                               { cometDocument = doc
                               , uVar = uqVar
                               , engineVerbose = sunroofVerbose opts
                               , compilerOpts = sunroofCompilerOpts opts
                               , timings = Nothing
                               }

-- | Default options to use for the sunroof comet server.
--
--   [@cometPort@] Default port is @3000@.
--
--   [@cometResourceBaseDir@] Default resource location is @"."@.
--
--   [@cometIndexFile@] Default index file is @"index.html"@.
--
--   [@cometOptions@] Uses the server path @/ajax@ for the
--     comet JSON communication. Sets verbosity to @0@ (quiet).
--
--   [@sunroofVerbose@] Is set to @0@ (quiet).
--
defaultServerOpts :: SunroofServerOptions
defaultServerOpts = SunroofServerOptions
  { cometPort = 3000
  , cometResourceBaseDir = "."
  , cometIndexFile = "index.html"
  , cometOptions = def { KC.prefix = "/ajax", KC.verbose = 0 }
  , sunroofVerbose = 0
  , sunroofCompilerOpts = def
  }

instance Default SunroofServerOptions where
  def = defaultServerOpts

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
  jsonToValue _ v = error $ "jsonToValue: JSON value is not unit: " ++ show v
  --toJS () = ()

instance SunroofResult JSBool where
  type ResultOf JSBool = Bool
  jsonToValue _ (Bool b) = b
  jsonToValue _ v = error $ "jsonToValue: JSON value is not a boolean: " ++ show v
  --toJS True = true
  --toJS False = false

instance SunroofResult JSNumber where
  type ResultOf JSNumber = Double
  jsonToValue _ (Number (I i)) = fromInteger i
  jsonToValue _ (Number (D d)) = d
  jsonToValue _ v = error $ "jsonToValue: JSON value is not a number: " ++ show v
  --toJS = JSNumber . Lit . show

instance SunroofResult JSString where
  type ResultOf JSString = String
  jsonToValue _ (String s) = unpack s
  jsonToValue _ v = error $ "jsonToValue: JSON value is not a string: " ++ show v
  --toJS = fromString

instance forall a . SunroofResult a => SunroofResult (JSArray a) where
  type ResultOf (JSArray a) = [ResultOf a]
  jsonToValue _ (Array ss) = map (jsonToValue (Proxy :: Proxy a)) $ V.toList ss
  jsonToValue _ v = error $ "jsonToValue: JSON value is not an array : " ++ show v
  --toJS = fromString


-- | Converts a JSON value to a Sunroof Javascript expression.
jsonToJS :: Value -> Expr
jsonToJS (Bool b)       = unbox $ js b
jsonToJS (Number (I i)) = unbox $ js i
jsonToJS (Number (D d)) = unbox $ js d
jsonToJS (String s)     = unbox $ js s
-- TODO: This is only a hack. Could null be a good reprensentation for unit '()'?
jsonToJS (Null)         = unbox $ nullJS
jsonToJS (Array arr)    = jsonArrayToJS arr
jsonToJS (Object obj)   = jsonObjectToJS obj

-- TODO: Some day find a Sunroof representation of this.
jsonObjectToJS :: Object -> Expr
jsonObjectToJS obj = literal $
  let literalMap = M.toList $ fmap (show . jsonToJS) obj
      convertKey k = "\"" ++ unpack k ++ "\""
      keyValues = fmap (\(k,v) -> convertKey k ++ ":" ++ v) literalMap
  in "{" ++ intercalate "," keyValues ++ "}"

-- TODO: Some day find a Sunroof representation of this.
jsonArrayToJS :: Array -> Expr
jsonArrayToJS arr = literal $
  "(new Array(" ++ (intercalate "," $ V.toList $ fmap (show . jsonToJS) arr) ++ "))"

instance SunroofValue Value where
  type ValueOf Value = JSObject
  js = box . jsonToJS

instance SunroofValue Text where
  type ValueOf Text = JSString
  js = string . unpack

-- -------------------------------------------------------------
-- Uplink and Downlink API
-- -------------------------------------------------------------

data Uplink a = Uplink SunroofEngine Int

newUplink :: SunroofEngine -> IO (Uplink a)
newUplink eng = do
  u <- docUniq eng
  return $ Uplink eng u

putUplink :: (Sunroof a) => Uplink a -> a -> JS t ()
putUplink (Uplink _ u) a = kc_reply (js u) a

getUplink :: forall a . (SunroofResult a) => Uplink a -> IO (ResultOf a)
getUplink (Uplink eng u) = do
  val <- KC.getReply (cometDocument eng) u
  -- TODO: make this throw an exception if it goes wrong (I supose error does this already)
  return $ jsonToValue (Proxy :: Proxy a) val

data Downlink a = Downlink SunroofEngine Int

newDownlink :: SunroofEngine -> IO (Downlink a)
newDownlink _eng = do undefined

putDownlink :: (Sunroof a) => Downlink a -> a -> IO ()
putDownlink = undefined

getDownlink :: forall a . (SunroofResult a) => Downlink a -> JS B (ResultOf a)
getDownlink = undefined

-- -------------------------------------------------------------
-- Comet Javascript API
-- -------------------------------------------------------------

kc_reply :: (Sunroof a) => JSNumber -> a -> JS t ()
kc_reply n a = fun "$.kc.reply" `apply` (n,a)

-- -------------------------------------------------------------
-- Debugging
-- -------------------------------------------------------------

debugSunroofEngine :: IO SunroofEngine
debugSunroofEngine = do
  doc <- KC.debugDocument
  uqVar <- atomically $ newTVar 0
  return $ SunroofEngine doc uqVar 3 def Nothing

data Timings a = Timings
        { compileTime :: !a        -- how long spent compiling
        , sendTime    :: !a        -- how long spent sending
        , waitTime    :: !a        -- how long spent waiting for a response
        }
        deriving Show

instance Functor Timings where
  fmap f (Timings t1 t2 t3) = Timings (f t1) (f t2) (f t3)

instance Semigroup a => Semigroup (Timings a) where
  (Timings t1 t2 t3) <> (Timings u1 u2 u3)  = Timings (t1<>u1) (t2<>u2) (t3<>u3)

newTimings :: SunroofEngine -> IO SunroofEngine
newTimings e = do
        v <- atomically $ newTVar $ Timings 0 0 0
        return $ e { timings = Just v }

resetTimings :: SunroofEngine -> IO ()
resetTimings (SunroofEngine { timings = Nothing }) = return ()
resetTimings (SunroofEngine { timings = Just t }) = atomically $ writeTVar t $ Timings 0 0 0

getTimings :: SunroofEngine -> IO (Timings NominalDiffTime)
getTimings (SunroofEngine { timings = Nothing }) = return $ Timings 0 0 0
getTimings (SunroofEngine { timings = Just t }) = atomically $ readTVar t

addCompileTime :: SunroofEngine -> UTCTime -> IO ()
addCompileTime (SunroofEngine { timings = Nothing }) _start = return ()
addCompileTime (SunroofEngine { timings = Just t }) start = do
        end <- getCurrentTime
        atomically $ modifyTVar t $ \ ts -> ts { compileTime = compileTime ts + diffUTCTime end start}
        return ()

addSendTime :: SunroofEngine -> UTCTime -> IO ()
addSendTime (SunroofEngine { timings = Nothing }) _start = return ()
addSendTime (SunroofEngine { timings = Just t }) start = do
        end <- getCurrentTime
        atomically $ modifyTVar t $ \ ts -> ts { sendTime = sendTime ts + diffUTCTime end start}
        return ()

addWaitTime :: SunroofEngine -> UTCTime -> IO ()
addWaitTime (SunroofEngine { timings = Nothing }) _start = return ()
addWaitTime (SunroofEngine { timings = Just t }) start = do
        end <- getCurrentTime
        atomically $ modifyTVar t $ \ ts -> ts { waitTime = waitTime ts + diffUTCTime end start}
        return ()
