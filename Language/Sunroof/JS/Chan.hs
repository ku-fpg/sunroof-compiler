
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

import Language.Sunroof.Classes ( Sunroof(..), SunroofArgument(..) )
import Language.Sunroof.Types
  ( T(..)
  , JS(..), JSB
  , JSTuple(..), JSFunction
  , SunroofThread
  , (#)
  , apply, new, reify
  , reifycc )
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

instance Sunroof (JSChan o) where
  box = JSChan . box
  unbox (JSChan o) = unbox o

instance (SunroofArgument o) => JSTuple (JSChan o) where
  type Internals (JSChan o) = ( (JSArray (JSFunction (JSFunction o ()) ())) -- callbacks of written data
                              , (JSArray (JSFunction o ()))                 -- callbacks of waiting readers
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
      (do f <- reify $ \ (k :: JSFunction a ()) -> apply k a :: JSB ()
          written # push (f :: JSFunction (JSFunction a ()) ())
      )
      (do f <- shift waiting
          forkJS (apply f a :: JSB ())
          return ()
      )

readChan :: forall a . (Sunroof a, SunroofArgument a) => JSChan a -> JS B a
readChan (match -> (written,waiting)) = do
  ifB ((written ! length') ==* 0)
      (do -- Add yourself to the 'waiting for writer' Q.
          reifycc $ \ k -> waiting # push (k :: JSFunction a ())
      )
      (do f <- shift written
          -- Here, we add our continuation into the written Q.
          reifycc $ \ k -> apply f k
      )



