{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Sunroof.KansasComet
  ( sync
  , sync'
  , async
  , wait
  , SunroofValue(..)
  , jsonToJS
  ) where

import Data.Aeson.Types ( Value(..), Object, Array )
import Data.Attoparsec.Number ( Number(..) )
import Data.Boolean
import Data.List ( intercalate )
import Data.String ( IsString(..) )
import Data.Text ( unpack )
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

import Language.Sunroof.Compiler
import Language.Sunroof.Types

-- export register
import Web.KansasComet
  ( Template(..)
  , extract
  --, register
  , Scope
  , send
  , Document, queryGlobal
  )

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
        call "$.kc.waitFor" <$> with ( string scope
                                     , object (show (map fst (extract tmpl)))
                                     , o
                                     )

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

