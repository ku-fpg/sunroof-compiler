
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Sunroof.JS.Chan
  ( JSChan
  , newChan
  , writeChan, readChan
  ) where

import Data.Boolean ( IfB(..), EqB(..) )

import Language.Sunroof.Classes
  ( Sunroof(..), SunroofArgument(..) )
import Language.Sunroof.Types
import Language.Sunroof.Concurrent ( forkJS )
import Language.Sunroof.Selector ( (!) )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Array
  ( JSArray
  , newArray, length'
  , push, shift )

-- -------------------------------------------------------------
-- JSChan Type
-- -------------------------------------------------------------

{-
data JSChan a = JSChan
        (JSArray (JSFunction (JSFunction a ()) ()))     -- callbacks of written data
        (JSArray (JSFunction a ()))                     -- callbacks of waiting readers
-}

newtype JSChan a = JSChan JSObject deriving Show

instance (SunroofArgument o) => Sunroof (JSChan o) where
  box = JSChan . box
  unbox (JSChan o) = unbox o

instance (SunroofArgument o) => JSTuple (JSChan o) where
  type Internals (JSChan o) = ( (JSArray (JSContinuation (JSContinuation o))) -- callbacks of written data
                              , (JSArray (JSContinuation o))                 -- callbacks of waiting readers
                              )
  match (JSChan o) = ( o ! "written", o ! "waiting" )
  tuple (written,waiting) = do
    o <- new "Object" ()
    o # "written" := written
    o # "waiting" := waiting
    return (JSChan o)

-- -------------------------------------------------------------
-- JSChan Combinators
-- -------------------------------------------------------------

newChan :: (SunroofArgument a) => JS t (JSChan a)
newChan = do
  written <- newArray ()
  waiting <- newArray ()
  tuple (written, waiting)

writeChan :: forall t a . (SunroofThread t, SunroofArgument a) => a -> JSChan a -> JS t ()
writeChan a (match -> (written,waiting)) = do
  ifB ((waiting ! length') ==* 0)
      (do f <- continuation $ \ (k :: JSContinuation a) -> goto k a :: JSB ()
          written # push (f :: JSContinuation (JSContinuation a))
      )
      (do f <- shift waiting
          forkJS (goto f a :: JSB ())
      )

readChan :: forall a . (Sunroof a, SunroofArgument a) => JSChan a -> JS B a
readChan (match -> (written,waiting)) = do
  ifB ((written ! length') ==* 0)
      (do -- Add yourself to the 'waiting for writer' Q.
          callcc $ \ k -> do waiting # push (k :: JSContinuation a)
                             done
      )
      (do f <- shift written
          -- Here, we add our continuation into the written Q.
          callcc $ \ k -> goto f k
      )



