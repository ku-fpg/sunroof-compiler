
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Sunroof.JS.MVar
  ( JSMVar
  , newMVar, newEmptyMVar
  , putMVar, takeMVar
  ) where

import Data.Boolean ( IfB(..), EqB(..) )

import Language.Sunroof.Classes
  ( Sunroof(..), SunroofArgument(..), SunroofValue(..) )
import Language.Sunroof.Types
  ( T(..)
  , JS(..), JSB
  , JSTuple(..), JSFunction, JSContinuation
  , (#)
  , apply, new, reify, goto, continuation
  , reifycc )
import Language.Sunroof.Concurrent ( forkJS )
import Language.Sunroof.Selector ( (!) )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Array
  ( JSArray
  , newArray, length'
  , push, shift )

-- -------------------------------------------------------------
-- JSMVar Type
-- -------------------------------------------------------------

{-
data JSMVar a = JSMVar
        (JSArray (JSFunction (JSFunction a ()) ()))     -- callbacks of written data
        (JSArray (JSFunction a ()))                     -- callbacks of waiting readers
-}

newtype JSMVar a = JSMVar JSObject deriving Show

instance (SunroofArgument o) => Sunroof (JSMVar o) where
  box = JSMVar . box
  unbox (JSMVar o) = unbox o

instance (SunroofArgument o) => JSTuple (JSMVar o) where
  type Internals (JSMVar o) = ( (JSArray (JSContinuation (JSContinuation o))) -- callbacks of written data
                              , (JSArray (JSContinuation o))                 -- callbacks of waiting readers
                              )
  match (JSMVar o) = ( o ! "written", o ! "waiting" )
  tuple (written,waiting) = do
    o <- new "Object" ()
    o # "written" := written
    o # "waiting" := waiting
    return (JSMVar o)

instance (SunroofArgument o) => SunroofValue (JSMVar o) where
  type ValueOf (JSMVar o) = JSMVar o
  js = id

-- -------------------------------------------------------------
-- JSMVar Combinators
-- -------------------------------------------------------------

newMVar :: (SunroofArgument a) => a -> JS B (JSMVar a)
newMVar a = do
  o <- newEmptyMVar
  o # putMVar a
  return o

newEmptyMVar :: (SunroofArgument a) => JS t (JSMVar a)
newEmptyMVar = do
  written <- newArray ()
  waiting <- newArray ()
  tuple (written, waiting)

-- Not quite right; pauses until someone bites
putMVar :: forall a . (SunroofArgument a) => a -> JSMVar a -> JS B ()
putMVar a (match -> (written,waiting)) = do
  ifB ((waiting ! length') ==* 0)
      (reifycc $ \ (k :: JSContinuation ()) -> do
            f <- continuation $ \ (kr :: JSContinuation a) -> do
                -- we've got a request for the contents
                -- so we can continue
                forkJS $ (goto k () :: JSB ())
                -- and send the boxed value
                goto kr a :: JSB ()
            written # push (f :: JSContinuation (JSContinuation a))
      )
      (do f <- shift waiting
          forkJS (goto f a :: JSB ())
          return ()
      )

takeMVar :: forall a . (Sunroof a, SunroofArgument a) => JSMVar a -> JS B a
takeMVar (match -> (written,waiting)) = do
  ifB ((written ! length') ==* 0)
      (do -- Add yourself to the 'waiting for writer' Q.
          reifycc $ \ k -> waiting # push (k :: JSContinuation a)
      )
      (do f <- shift written
          -- Here, we add our continuation into the written Q.
          reifycc $ \ k -> goto f k
      )


