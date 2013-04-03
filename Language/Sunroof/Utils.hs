{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Common utilities for Sunroof.
module Language.Sunroof.Utils
  ( comment, jsfix
  ) where

import Language.Sunroof.Classes
import Language.Sunroof.Types

-- -------------------------------------------------------------
-- Comments
-- -------------------------------------------------------------

comment :: String -> JS t ()
comment = single . JS_Comment

-- -------------------------------------------------------------
-- Fixpoint combinator
-- -------------------------------------------------------------

-- | @jsfix@ is the @mfix@ for the JS Monad.
jsfix :: (SunroofArgument a) => (a -> JSA a) -> JSA a
jsfix = single . JS_Fix
