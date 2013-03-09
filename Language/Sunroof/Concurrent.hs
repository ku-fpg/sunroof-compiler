{-# LANGUAGE OverloadedStrings, DataKinds, ScopedTypeVariables, TypeFamilies, ViewPatterns #-}

module Language.Sunroof.Concurrent where

import Data.Boolean

import Language.Sunroof.Types
--import Language.Sunroof.Types (T(A,B))
import Language.Sunroof.Classes ( Sunroof(..) )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Browser ( window, setTimeout )

loopJS :: (Sunroof a) => a -> (a -> JSB a) -> JSB ()      -- does not terminate
loopJS start m = do
    v <- newJSRef (cast nullJS)
    s <- newJSRef start
    f <- continuation $ \ () -> do
            a <- readJSRef s
            a' <- m a
            writeJSRef s a'
            f <- readJSRef v
            _ <- liftJS $ window # setTimeout f 0
            return ()
    writeJSRef v f
    apply f ()  -- and call the function
    return ()


forkJS :: JSThread t => JS t () -> JS t2 ()
forkJS m = do
        f <- function' $ \ () -> m
        _ <- liftJS $ window # setTimeout f 0
        return ()


threadDelayJSB :: JSNumber -> JSB ()
threadDelayJSB n = reifyccJS $ \ o -> do
        _ <- liftJS $ window # setTimeout o n
        return ()

yieldJSB :: JSB ()
yieldJSB = threadDelayJSB 0

--------------------------------------------------------------------------------------

{-
data JSChan a = JSChan
        (JSArray (JSFunction (JSFunction a ()) ()))     -- callbacks of written data
        (JSArray (JSFunction a ()))                     -- callbacks of waiting readers
-}

newtype JSChan a = JSChan JSObject
        deriving Show

instance Sunroof (JSChan o) where
        box = JSChan . box
        unbox (JSChan o) = unbox o

instance (JSArgument o) => JSTuple (JSChan o) where
        type Internals (JSChan o) = ( (JSArray (JSFunction (JSFunction o ()) ()))     -- callbacks of written data
                                    , (JSArray (JSFunction o ()))                     -- callbacks of waiting readers
                                    )

        match (JSChan o) = ( o ! "written", o ! "waiting" )
        tuple (written,waiting) = do
                o <- new "Object" ()
                o # "written" := written
                o # "waiting" := waiting
                return (JSChan o)

newChan :: (JSArgument a) => JS t (JSChan a)
newChan = do
        written <- newArray
        waiting <- newArray
        tuple (written, waiting)

writeChan :: forall t a . (JSThread t, JSArgument a) => a -> JSChan a -> JS t ()
writeChan a (match -> (written,waiting)) = do
        ifB (lengthArray waiting ==* 0)
            (do f <- function' $ \ (k :: JSFunction a ()) -> apply k a :: JSB ()
                written # pushArray (f :: JSFunction (JSFunction a ()) ())
            )
            (do f <- shiftArray waiting
                forkJS (apply f a :: JSB ())
                return ()
            )

readChan :: forall a . (Sunroof a, JSArgument a) => JSChan a -> JS B a
readChan (match -> (written,waiting)) = do
        ifB (lengthArray written ==* 0)
            (do -- Add yourself to the 'waiting for writer' Q.
                reifyccJS $ \ k -> waiting # pushArray (k :: JSFunction a ())
            )
            (do f <- shiftArray written
                -- Here, we add our continuation into the written Q.
                reifyccJS $ \ k -> apply f k
            )


{-
-}











