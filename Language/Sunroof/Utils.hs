{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Common utilities for Sunroof.
module Language.Sunroof.Utils
  ( comment, fixJS
  ) where

import Language.Sunroof.Classes
import Language.Sunroof.Types

-- -------------------------------------------------------------
-- Comments
-- -------------------------------------------------------------

-- | Write a JavaScript comment into the generated source.
comment :: String -> JS t ()
comment = single . JS_Comment

-- -------------------------------------------------------------
-- Fixpoint combinator
-- -------------------------------------------------------------

-- | @jsfix@ is the @mfix@ for the JS Monad.
fixJS :: (SunroofArgument a) => (a -> JSA a) -> JS t a
fixJS = single . JS_Fix
