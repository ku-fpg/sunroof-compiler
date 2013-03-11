
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

import Language.Sunroof.Classes ( Sunroof(..), JSArgument(..) )
import Language.Sunroof.Types
  ( T(..)
  , JS(..), JSB
  , JSTuple(..), JSFunction
  , (#)
  , apply, new, reify
  , reifyccJS )
import Language.Sunroof.Concurrent ( forkJS )
import Language.Sunroof.Selector ( (!) )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.Array
  ( JSArray
  , newArray, lengthArray
  , pushArray, shiftArray )

-- -------------------------------------------------------------
-- JSMVar Type
-- -------------------------------------------------------------

{-
data JSMVar a = JSMVar
        (JSArray (JSFunction (JSFunction a ()) ()))     -- callbacks of written data
        (JSArray (JSFunction a ()))                     -- callbacks of waiting readers
-}

newtype JSMVar a = JSMVar JSObject deriving Show

instance Sunroof (JSMVar o) where
  box = JSMVar . box
  unbox (JSMVar o) = unbox o

instance (JSArgument o) => JSTuple (JSMVar o) where
  type Internals (JSMVar o) = ( (JSArray (JSFunction (JSFunction o ()) ())) -- callbacks of written data
                              , (JSArray (JSFunction o ()))                 -- callbacks of waiting readers
                              )
  match (JSMVar o) = ( o ! "written", o ! "waiting" )
  tuple (written,waiting) = do
    o <- new "Object" ()
    o # "written" := written
    o # "waiting" := waiting
    return (JSMVar o)

-- -------------------------------------------------------------
-- JSMVar Combinators
-- -------------------------------------------------------------

newMVar :: (JSArgument a) => a -> JS B (JSMVar a)
newMVar a = do
  o <- newEmptyMVar
  o # putMVar a
  return o

newEmptyMVar :: (JSArgument a) => JS t (JSMVar a)
newEmptyMVar = do
  written <- newArray ()
  waiting <- newArray ()
  tuple (written, waiting)


-- Not quite right; pauses until someone bites
putMVar :: forall a . (JSArgument a) => a -> JSMVar a -> JS B ()
putMVar a (match -> (written,waiting)) = do
  ifB (lengthArray waiting ==* 0)
      (reifyccJS $ \ (k :: JSFunction () ()) -> do
            f <- reify $ \ (kr :: JSFunction a ()) -> do
                -- we've got a request for the contents
                -- so we can continue
                forkJS $ (apply k () :: JSB ())
                -- and send the boxed value
                apply kr a :: JSB ()
            written # pushArray (f :: JSFunction (JSFunction a ()) ())
      )
      (do f <- shiftArray waiting
          forkJS (apply f a :: JSB ())
          return ()
      )

takeMVar :: forall a . (Sunroof a, JSArgument a) => JSMVar a -> JS B a
takeMVar (match -> (written,waiting)) = do
  ifB (lengthArray written ==* 0)
      (do -- Add yourself to the 'waiting for writer' Q.
          reifyccJS $ \ k -> waiting # pushArray (k :: JSFunction a ())
      )
      (do f <- shiftArray written
          -- Here, we add our continuation into the written Q.
          reifyccJS $ \ k -> apply f k
      )



