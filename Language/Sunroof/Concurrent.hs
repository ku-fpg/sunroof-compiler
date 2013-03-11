
module Language.Sunroof.Concurrent
  ( loop
  , forkJS
  , threadDelay
  , yield
  ) where

import Language.Sunroof.Types
  ( JSB, JS
  , SunroofThread
  , continuation, reify
  , apply, cast, nullJS
  , (#)
  , liftJS, reifyccJS )
import Language.Sunroof.Classes ( Sunroof(..) )
import Language.Sunroof.JS.Ref ( newJSRef, readJSRef, writeJSRef )
import Language.Sunroof.JS.Number ( JSNumber )
import Language.Sunroof.JS.Browser ( window, setTimeout )

-- -------------------------------------------------------------
-- General Concurrent Combinators.
-- -------------------------------------------------------------

-- | @loopJS x f@ executes the function @f@ repeatedly.
--   After each iteration the result value of the function
--   is feed back as input of the next iteration.
--   The initial value supplied for the first iteration is @x@.
--   This loop will never terminate.
loop :: (Sunroof a) => a -> (a -> JSB a) -> JSB ()
loop start m = do
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
  apply f () -- and call the function
  return ()

-- | Fork of another thread of execution.
forkJS :: SunroofThread t => JS t () -> JS t2 ()
forkJS m = do
  f <- reify $ \ () -> m
  _ <- liftJS $ window # setTimeout f 0
  return ()

-- | Delay the execution of all instructions after this one by
--   the given amount of milliseconds.
threadDelay :: JSNumber -> JSB ()
threadDelay n = reifyccJS $ \ o -> do
  _ <- liftJS $ window # setTimeout o n
  return ()

-- | Give another thread time to execute.
yield :: JSB ()
yield = threadDelay 0













