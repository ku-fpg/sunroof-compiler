
{-# LANGUAGE TypeFamilies #-}

module Language.Sunroof.Classes
  ( Sunroof(..)
  , SunroofValue(..)
  ) where

import Data.Proxy ( Proxy )

import Language.Sunroof.JavaScript ( Expr, E(Lit), Type(Base,Unit) )

-- -------------------------------------------------------------
-- Sunroof Type Class
-- -------------------------------------------------------------

class Show a => Sunroof a where
  box :: Expr -> a
  unbox :: a -> Expr

  showVar :: a -> String -- needed because show instance for unit is problematic
  showVar = show

  typeOf :: Proxy a -> Type
  typeOf _ = Base

-- unit is the oddball
instance Sunroof () where
--  showVar _ = ""
  box _ = ()
  unbox () = Lit "null"
  typeOf _ = Unit

-- -------------------------------------------------------------
-- SunroofValue Type Class
-- -------------------------------------------------------------

class SunroofValue a where
  type ValueOf a :: *
  js :: a -> ValueOf a

instance SunroofValue () where
  type ValueOf () = ()
  js () = ()








