
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Abstraction over the most general type in Javascript.
module Language.Sunroof.JS.Object
  ( JSObject
  , object
  , this
  ) where

import Data.Boolean ( BooleanOf, IfB(..), EqB(..) )

import Language.Sunroof.JavaScript ( Expr, showExpr, literal, binOp )
import Language.Sunroof.Classes ( Sunroof(..) )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )

-- -------------------------------------------------------------
-- JSObject Type
-- -------------------------------------------------------------

-- | Data type for all Javascript objects.
data JSObject = JSObject Expr

instance Show JSObject where
  show (JSObject v) = showExpr False v

instance Sunroof JSObject where
  box = JSObject
  unbox (JSObject o) = o

type instance BooleanOf JSObject = JSBool

instance IfB JSObject where
  ifB = jsIfB

-- | Reference equality, not value equality.
instance EqB JSObject where
  (JSObject a) ==* (JSObject b) = box $ binOp "==" a b

-- -------------------------------------------------------------
-- JSObject Combinators
-- -------------------------------------------------------------

-- | Create an arbitrary object from a literal in form of a string.
object :: String -> JSObject
object = box . literal

-- | The @this@ reference.
this :: JSObject
this = object "this"
