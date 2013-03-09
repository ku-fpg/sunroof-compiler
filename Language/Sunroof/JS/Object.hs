
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Sunroof.JS.Object
  ( JSObject
  ) where

import Data.Boolean ( BooleanOf, IfB(..) )

import Language.Sunroof.JavaScript ( Expr, showExpr )
import Language.Sunroof.Classes ( Sunroof(..), SunroofValue(..) )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

-- -------------------------------------------------------------
-- JSObject Type
-- -------------------------------------------------------------

data JSObject = JSObject Expr

instance Show JSObject where
  show (JSObject v) = showExpr False v

instance Sunroof JSObject where
  box = JSObject
  unbox (JSObject o) = o

type instance BooleanOf JSObject = JSBool

instance IfB JSObject where
  ifB = jsIfB

instance SunroofValue Expr where
  type ValueOf Expr = JSObject
  js = box
