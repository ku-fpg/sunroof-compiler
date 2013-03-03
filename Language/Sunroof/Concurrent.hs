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
