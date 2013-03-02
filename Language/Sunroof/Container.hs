{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Language.Sunroof.Container
        ( -- * JSRef's

          JSRef
        , newJSRef
        , readJSRef
        , writeJSRef
        ) where

import Language.Sunroof.Types
import Language.Sunroof.Types (T(A,B))

-- | This is the IORef of Sunroof.
newtype JSRef a = JSRef JSObject

newJSRef :: (Sunroof a) => a -> JS t (JSRef a)
newJSRef a = do
        obj <- new
        obj # "val" := a
        return $ JSRef obj

-- | This a a non-blocking read
readJSRef :: (Sunroof a) => JSRef a -> JS t a
readJSRef (JSRef obj) = evaluate $ obj ! "val"

-- | This a a non-blocking write
writeJSRef :: (Sunroof a) => JSRef a -> a -> JS t ()
writeJSRef (JSRef obj) a = obj # "val" := a

modifyJSRef :: (Sunroof a) => JSRef a -> (a -> JS A a) -> JS A ()
modifyJSRef ref f = do
        val <- readJSRef ref
        f val >>= writeJSRef ref
