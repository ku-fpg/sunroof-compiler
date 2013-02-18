{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Language.Sunroof.KansasComet
  ( sync
  , sync'
  , async
  , wait
  ) where

import Data.Aeson.Types ( Value(..), Object, Array )
import Data.Attoparsec.Number ( Number(..) )
import Data.Boolean
import Data.List ( intercalate )
import Data.String ( IsString(..) )
import Data.Text ( unpack )
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

-- Async requests that something be done, without waiting for any reply
async :: Document -> JS () -> IO ()
async doc jsm = do
  let (res,_) = compileJS jsm
  --print res
  send doc $ res  -- send it, and forget it
  return ()

sync' :: (Sunroof a) => Document -> JS a -> IO (Maybe Value)
sync' doc jsm = do
  let (res,retVar) = compileJS jsm
  --print (res,retVar)
  case retVar of
    -- This is async:
    "" -> do
      send doc $ res -- send it, and forget it
      return $ Nothing
    ret -> Just `fmap` queryGlobal doc (res,ret)

-- Sync requests that something be done, *and* waits for a reply.
sync :: (Sunroof a) => Document -> JS a -> IO (Maybe a)
sync doc jsm = do
  value <- sync' doc jsm
  return $ fmap (box  . jsonToJS) value

-- This can be build out of primitives
wait :: Scope -> Template event -> (JSObject -> JS ()) -> JS ()
wait scope tmpl k = do
        o <- function k
        call "$.kc.waitFor" <$> with ( string scope
                                     , object (show (map fst (extract tmpl)))
                                     , o
                                     )

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

