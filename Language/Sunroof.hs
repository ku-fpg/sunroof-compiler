{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, KindSignatures, GADTs #-}
module Language.Sunroof where

import Control.Monad.Operational

import Language.Sunroof.Compiler
import Language.Sunroof.Types

-- export register
import Web.KansasComet (Template(..), extract, register, Scope, send, Document)

-- Async requests that something be done, without waiting for any reply
async :: Document -> JS () -> IO ()
async doc jsm = do
        let (res,_) = compileJS jsm
        send doc $ res  -- send it, and forget it
        return ()

-- Sync requests that something be done, *and* waits for a reply.
sync :: (Sunroof a) => Document -> JS a -> IO a
sync doc jsm = do
        let (res,ret) = compileJS jsm
        print (res,ret)
        case ret of
          "" -> return $ undefined
          ret -> do
            send doc $ concat [ res
                              , "$.kc.reply(" 
                              , show 93272353275 -- Should be: (secret doc)
                              , "," 
                              , ret 
                              , ");"
                              ]
            -- val <- getReply doc (secret doc) -- Not possible because privat.
            -- TODO: Convert: Value -> JSValue
            -- TODO: Return the value.
            return $ undefined

-- This can be build out of primitives
wait :: Scope -> Template event -> (JSObject -> JS ()) -> JS ()
wait scope tmpl k = do
        o <- function k
        call "$.kc.waitFor" <$> with [ cast (string scope)
                                     , cast (object (show (map fst (extract tmpl))))
                                     , cast o]


