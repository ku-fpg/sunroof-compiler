{-# LANGUAGE OverloadedStrings, DataKinds, ScopedTypeVariables #-}

module Language.Sunroof.Concurrent where

import Data.Boolean

import Language.Sunroof.Types
import Language.Sunroof.Types (T(A,B))
import Language.Sunroof.JS.Browser(window,setTimeout,alert)



loopJS :: (Sunroof a) => a -> (a -> JSB a) -> JSB ()      -- does not terminate
loopJS start m = do
    v <- newJSRef (cast nullJS)
    s <- newJSRef start
    f <- continuation $ \ () -> do
            a <- readJSRef s
            a' <- m a
            writeJSRef s a'
            f <- readJSRef v
            liftJS $ window # setTimeout f 0
            return ()
    writeJSRef v f
    apply f ()  -- and call the function
    return ()


forkJS :: JSThread t => JS t () -> JS t2 ()
forkJS m = do
        f <- function' $ \ () -> m
        liftJS $ window # setTimeout f 0
        return ()


threadDelayJSB :: JSNumber -> JSB ()
threadDelayJSB n = reifyccJS $ \ o -> do
        liftJS $ window # setTimeout o n
        return ()

yieldJSB :: JSB ()
yieldJSB = threadDelayJSB 0


data JSChan a = JSChan
        (JSArray (JSFunction (JSFunction a ()) ()))     -- callbacks of written data
        (JSArray (JSFunction a ()))                     -- callbacks of waiting readers

newChan :: (JSArgument a) => JS t (JSChan a)
newChan = do
        written <- newArray
        waiting <- newArray
        return $ JSChan written waiting


writeChan :: forall t a . (JSThread t, JSArgument a) => JSChan a -> a -> JS t ()
writeChan (JSChan written waiting) a = do
        ifB (lengthArray waiting ==* 0)
            (do f <- function' $ \ (k :: JSFunction a ()) -> apply k a :: JS B ()
                written # pushArray (f :: JSFunction (JSFunction a ()) ())
            )
            (do f <- shiftArray waiting
                apply f a       -- actually runs this thread for a bit.
                                -- remember, it can block, but the underlying
                                -- javascript will yield.
                                -- not sure if this could cause race conditions?
                                -- There *is* no blocking, so I think not.
                return ()
            )
readChan :: forall a . (Sunroof a, JSArgument a) => JSChan a -> JS B a
readChan (JSChan written waiting) = do
        ifB (lengthArray written ==* 0)
            (do -- Add yourself to the 'waiting for writer' Q.
                reifyccJS $ \ k -> waiting # pushArray (k :: JSFunction a ())
            )
            (do f <- shiftArray written
                -- Here, we add our continuation into the written Q.
                reifyccJS $ \ k -> apply f k
            )


{-
- reading a channel can block

readChan (JSChan obj) = ThreadedJS $ \ k -> do
        --- first, figure out if there is anything in the queue
        sends :: JSArray JSObject <- evaluate $ obj ! "sends"
        recvs :: JSArray JSObject <- evaluate $ obj ! "recvs"
        ifB (lengthJS sends ==* 0)
            (do -- if empty, add youself to a callback queue
                -- push(
                return ()
            )
            (do -- if not empty, take the next thing off the channel,
                -- and return it (using the continuation)
--                fn <- recvs.pop()
--                apply
                return ()
            )

--        pop(obj ! "recvsn")

-}











