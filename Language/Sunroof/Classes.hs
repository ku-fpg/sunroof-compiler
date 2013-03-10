
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Sunroof.Classes
  ( Sunroof(..)
  , SunroofValue(..)
  , JSArgument(..)
  , UniqM(..), Uniq
  , mkVar, jsVar
  ) where

import Control.Monad ( ap, liftM2, liftM3, liftM4, liftM5 )

import Data.Proxy ( Proxy(Proxy) )

import Language.Sunroof.JavaScript ( Expr, E(Var), Type(Base,Unit), literal )

-- -------------------------------------------------------------
-- UniqM Type Class
-- -------------------------------------------------------------

type Uniq = Int -- used as a unique label

class Monad m => UniqM m where
  uniqM :: m Uniq

mkVar :: Sunroof a => Uniq -> a
mkVar = box . Var . ("v" ++) . show

jsVar :: (Sunroof a, UniqM m) => m a
jsVar = uniqM >>= return . mkVar

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
  unbox () = literal "null"
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

-- -------------------------------------------------------------
-- JSArgument Type Class
-- -------------------------------------------------------------

class JSArgument args where
  jsArgs   :: args -> [Expr]        -- turn a value into a list of expressions
  jsValue  :: (UniqM m) => m args
  typesOf  :: Proxy args -> [Type]

instance Sunroof a => JSArgument a where
  jsArgs a = [unbox a]
  jsValue = jsVar
  typesOf p = [typeOf p]

instance JSArgument () where
  jsArgs _ = []
  jsValue = return ()
  typesOf _ = []

instance (Sunroof a, Sunroof b) => JSArgument (a,b) where
  jsArgs ~(a,b) = [unbox a, unbox b]
  jsValue = liftM2 (,) jsVar jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a),typeOf (Proxy :: Proxy b)]

instance (Sunroof a, Sunroof b, Sunroof c) => JSArgument (a,b,c) where
  jsArgs ~(a,b,c) = [unbox a, unbox b, unbox c]
  jsValue = liftM3 (,,) jsVar jsVar jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ]

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d) => JSArgument (a,b,c,d) where
  jsArgs ~(a,b,c,d) = [unbox a, unbox b, unbox c, unbox d]
  jsValue = liftM4 (,,,) jsVar jsVar jsVar jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ]

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e) => JSArgument (a,b,c,d,e) where
  jsArgs ~(a,b,c,d,e) = [unbox a, unbox b, unbox c, unbox d, unbox e]
  jsValue = liftM5 (,,,,) jsVar jsVar jsVar jsVar jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ,typeOf (Proxy :: Proxy e)
                  ]

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f) => JSArgument (a,b,c,d,e,f) where
  jsArgs ~(a,b,c,d,e,f) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f]
  jsValue = return (,,,,,) `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ,typeOf (Proxy :: Proxy e)
                  ,typeOf (Proxy :: Proxy f)
                  ]

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f, Sunroof g) => JSArgument (a,b,c,d,e,f,g) where
  jsArgs ~(a,b,c,d,e,f,g) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f, unbox g]
  jsValue = return (,,,,,,) `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ,typeOf (Proxy :: Proxy e)
                  ,typeOf (Proxy :: Proxy f)
                  ,typeOf (Proxy :: Proxy g)
                  ]

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f, Sunroof g, Sunroof h) => JSArgument (a,b,c,d,e,f,g,h) where
  jsArgs ~(a,b,c,d,e,f,g,h) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f, unbox g, unbox h]
  jsValue = return (,,,,,,,) `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ,typeOf (Proxy :: Proxy e)
                  ,typeOf (Proxy :: Proxy f)
                  ,typeOf (Proxy :: Proxy g)
                  ,typeOf (Proxy :: Proxy h)
                  ]

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f, Sunroof g, Sunroof h, Sunroof i) => JSArgument (a,b,c,d,e,f,g,h,i) where
  jsArgs ~(a,b,c,d,e,f,g,h,i) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f, unbox g, unbox h, unbox i]
  jsValue = return (,,,,,,,,)
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
                        `ap` jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ,typeOf (Proxy :: Proxy e)
                  ,typeOf (Proxy :: Proxy f)
                  ,typeOf (Proxy :: Proxy g)
                  ,typeOf (Proxy :: Proxy h)
                  ,typeOf (Proxy :: Proxy i)
                  ]






