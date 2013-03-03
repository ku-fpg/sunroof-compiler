{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Language.Sunroof.Concurrent where

import Language.Sunroof.Types
import Language.Sunroof.Types (T(A,B))
import Language.Sunroof.JS.Browser(window,setTimeout)

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
