{-# LANGUAGE ScopedTypeVariables #-}

-- | Provides common combinators for concurrency in Javascript.
--
--   The emulated threading Javascript threading model provided by
--   Sunroof is based on cooperative multithreading
--   (since Javascript is not multithreaded).
module Language.Sunroof.Concurrent
  ( loop
  , forkJS
  , threadDelay
  , yield
  ) where

import Language.Sunroof.Types
import Language.Sunroof.Classes ( Sunroof(..) )
import Language.Sunroof.JS.Ref ( newJSRef, readJSRef, writeJSRef, JSRef )
import Language.Sunroof.JS.Number ( JSNumber )
import Language.Sunroof.JS.Browser ( window, setTimeout )

-- -------------------------------------------------------------
-- General Concurrent Combinators.
-- -------------------------------------------------------------

-- | @loop x f@ executes the function @f@ repeatedly.
--   After each iteration the result value of the function
--   is feed back as input of the next iteration.
--   The initial value supplied for the first iteration is @x@.
--   This loop will never terminate.
loop :: (Sunroof a) => a -> (a -> JSB a) -> JSB ()
loop start m = do
  v :: JSRef (JSContinuation ()) <- newJSRef (cast nullJS)
  s <- newJSRef start
  f <- continuation $ \ () -> do
          a <- readJSRef s
          a' <- m a
          s # writeJSRef a'
          f <- readJSRef v
          _ <- liftJS $ window # setTimeout (\x -> goto f x) 0
          return ()
  v # writeJSRef f
  _ <- goto f () -- and call the function
  return ()

-- | Fork of the given computation in a different thread.
forkJS :: (SunroofThread t1) => JS t1 () -> JS t2 ()
forkJS m = do
  _ <- window # setTimeout (\() -> blockableJS m) 0
  return ()

-- | Delay the execution of all instructions after this one by
--   the given amount of milliseconds.
threadDelay :: JSNumber -> JSB ()
threadDelay n = callcc $ \ o -> do
  _ <- window # setTimeout (\x -> goto o x) n
  done

-- | Give another thread time to execute.
yield :: JSB ()
yield = threadDelay 0













