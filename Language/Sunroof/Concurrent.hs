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
import Language.Sunroof.Classes
import Language.Sunroof.JS.Number ( JSNumber )
import Language.Sunroof.JS.Browser ( window, setTimeout )
import Language.Sunroof.Utils

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
  f <- fixJS $ \ f -> continuation $ \ a -> do
          a' <- m a
          yield -- stop after every loop for pause
          goto f a'
  goto f start -- and call the looping function

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













