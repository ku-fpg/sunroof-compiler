{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Language.Sunroof.Concurrent where

import Data.Boolean

import Language.Sunroof.Types
import Language.Sunroof.Types (T(A,B))
import Language.Sunroof.JS.Browser(window,setTimeout,alert)



loopJS :: JSB () -> JSB ()      -- does not terminate
loopJS m = do
    v <- newJSRef (cast nullJS)
    f <- continuation $ \ () -> do
            m
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

-- break out of the JSB
stopJSB :: JSB ()
stopJSB = reifyccJS $ \ _ -> return ()

data JSChan a = JSChan (JSArray (JSFunction a ())) (JSArray ())


writeChan :: (JSThread t, JSArgument a) => JSChan a -> a -> JS t ()
writeChan (JSChan recvs sends) a = do
        ifB (lengthArray recvs ==* 0)
            (do alert ("no one listening")
            )
            (do f <- popArray recvs
                apply f a       -- actually runs this thread for a bit.
                                -- remember, it can block, but the underlying
                                -- javascript will yield.
                                -- not sure if this could cause race conditions?
                                -- There *is* no blocking, so I think not.
                return ()
            )


{-
- reading a channel can block
readChan :: JSChan a -> ThreadedJS a
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











