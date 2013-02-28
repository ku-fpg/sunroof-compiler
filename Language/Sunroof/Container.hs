{-# LANGUAGE OverloadedStrings #-}

module Language.Sunroof.Container
        ( -- * JSRef's

          JSRef
        , newJSRef
        , readJSRef
        , writeJSRef
        ) where

import Language.Sunroof.Types

-- | This is the IORef of Sunroof.
newtype JSRef a = JSRef JSObject

newJSRef :: (Sunroof a) => a -> JS (JSRef a)
newJSRef a = do
        obj <- new
        obj # "val" := a
        return $ JSRef obj

-- | This a a non-blocking read
readJSRef :: (Sunroof a) => JSRef a -> JS a
readJSRef (JSRef obj) = evaluate $ obj ! "val"

-- | This a a non-blocking write
writeJSRef :: (Sunroof a) => JSRef a -> a -> JS ()
writeJSRef (JSRef obj) a = obj # "val" := a

modifyJSRef :: (Sunroof a) => JSRef a -> (a -> JS a) -> JS ()
modifyJSRef ref f = do
        val <- readJSRef ref
        f val >>= writeJSRef ref
