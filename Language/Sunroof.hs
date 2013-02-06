{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs #-}
module Language.Sunroof where

--import Control.Monad.Operational

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
        send doc $ res  -- send it, and forget it
        return ()

-- Sync requests that something be done, *and* waits for a reply.
sync :: (Sunroof a) => Document -> JS a -> IO a
sync doc jsm = do
        let (res,retVar) = compileJS jsm
        print (res,retVar)
        case retVar of
          "" -> return $ undefined
          ret -> do
            value <- queryGlobal doc (res,ret)
            return $ (cast . jsonToJS) value
            --send doc $ concat [ res, "; $.kc.reply(", documentId doc, ",", ret, ");"]
            --val <- getReply doc (documentId doc) -- Not possible because privat.
            -- return $ jsonToJS val

-- This can be build out of primitives
wait :: Scope -> Template event -> (JSObject -> JS ()) -> JS ()
wait scope tmpl k = do
        o <- function k
        call "$.kc.waitFor" <$> with [ cast (string scope)
                                     , cast (object (show (map fst (extract tmpl))))
                                     , cast o]

jsonToJS :: Value -> JSValue
jsonToJS (Bool b)       = cast (if b then false else true :: JSBool)
jsonToJS (Number (I i)) = cast (fromInteger i :: JSNumber)
jsonToJS (Number (D d)) = cast (fromRational (toRational d) :: JSNumber)
jsonToJS (String s)     = cast (fromString (unpack s) :: JSString)
-- TODO: This is only a hack. Could null be a good reprensentation for unit '()'?
jsonToJS (Null)         = box $ Lit "null" 
jsonToJS (Array arr)    = jsonArrayToJS arr
jsonToJS (Object obj)   = jsonObjectToJS obj

-- TODO: Some day find a Sunroof representation of this.
jsonObjectToJS :: Object -> JSValue
jsonObjectToJS obj = box $ Lit $ 
  let literalMap = M.toList $ fmap (showVar . jsonToJS) obj
      convertKey k = "\"" ++ unpack k ++ "\""
      keyValues = fmap (\(k,v) -> convertKey k ++ ":" ++ v) literalMap
  in "{" ++ intercalate "," keyValues ++ "}"

-- TODO: Some day find a Sunroof representation of this.
jsonArrayToJS :: Array -> JSValue
jsonArrayToJS arr = box $ Lit $ 
  "(new Array(" ++ (intercalate "," $ V.toList $ fmap (showVar . jsonToJS) arr) ++ "))"










