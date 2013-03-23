
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | 'JSMVar' provides the same functionality and 
--   concurrency abstraction Javascript computations
--   as 'Control.Concurrent.MVar' in Haskell.
module Language.Sunroof.JS.MVar
  ( JSMVar
  , newMVar, newEmptyMVar
  , putMVar, takeMVar
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
-- JSMVar Type
-- -------------------------------------------------------------

-- | 'JSMVar' abstraction. The type parameter gives 
--   the type of values held in a 'JSMVar'.
newtype JSMVar a = JSMVar JSObject deriving Show

-- | They are first-class Javascript values.
instance (SunroofArgument o) => Sunroof (JSMVar o) where
  box = JSMVar . box
  unbox (JSMVar o) = unbox o

-- | They contain different parts and can be decomposed.
--   You should not mess with their internals.
instance (SunroofArgument o) => JSTuple (JSMVar o) where
  type Internals (JSMVar o) = 
    ( (JSArray (JSContinuation (JSContinuation o))) -- callbacks of written data
    , (JSArray (JSContinuation o))                 -- callbacks of waiting readers
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

-- | Create a new 'JSMVar' with the given value inside.
--   See 'newEmptyMVar'.
newMVar :: (SunroofArgument a) => a -> JS B (JSMVar a)
newMVar a = do
  o <- newEmptyMVar
  o # putMVar a
  return o

-- | Create a new empty 'JSMVar'.
--   See 'newMVar'.
newEmptyMVar :: (SunroofArgument a) => JS t (JSMVar a)
newEmptyMVar = do
  written <- newArray ()
  waiting <- newArray ()
  tuple (written, waiting)

-- TODO: Not quite right; pauses until someone bites
-- | Put the value into the 'JSMVar'. If there already is a 
--   value inside, this will block until it is taken out.
putMVar :: forall a . (SunroofArgument a) => a -> JSMVar a -> JS B ()
putMVar a (match -> (written,waiting)) = do
  ifB ((waiting ! length') ==* 0)
      (-- no-one is waiting, so check for fullness
       ifB ((written ! length') ==* 0)
          (-- mvar empty, so just push and continue
           do f <- continuation $ \ (k :: JSContinuation a) -> goto k a :: JSB ()
              written # push (f :: JSContinuation (JSContinuation a))
          )
          (-- mvar full, so block
           callcc $ \ (k :: JSContinuation ()) -> do
            f <- continuation $ \ (kr :: JSContinuation a) -> do
                -- we've got a request for the contents
                -- so we can continue
                forkJS $ (goto k () :: JSB ())
                -- and send the boxed value
                goto kr a :: JSB ()
            written # push (f :: JSContinuation (JSContinuation a))
            done
         )
      )
        -- If someone is already waiting, then just pass the value (and continue without pausing)
      (do f <- shift waiting
          forkJS (goto f a :: JSB ())
          return ()
      )

-- | Take the value out of the 'JSMVar'. If there is no value
--   inside, this will block until one is available.
takeMVar :: forall a . (Sunroof a, SunroofArgument a) => JSMVar a -> JS B a
takeMVar (match -> (written,waiting)) = do
  ifB ((written ! length') ==* 0)
      (do -- Add yourself to the 'waiting for writer' Q.
          callcc $ \ k -> do waiting # push (k :: JSContinuation a)
                             done
      )
      (do f <- shift written
          -- Here, we add our continuation into the written Q.
          callcc $ \ k -> goto f k
      )


