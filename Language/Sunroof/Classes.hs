
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
#if !(MIN_VERSION_base(4,8,0))
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Provides the central type classes used by Sunroof.
module Language.Sunroof.Classes
  ( Sunroof(..)
  , SunroofValue(..)
  , SunroofArgument(..)
  , UniqM(..), Uniq
  , mkVar, jsVar
  ) where

import Control.Monad ( ap, liftM2, liftM3, liftM4, liftM5 )

import Data.Proxy ( Proxy(Proxy) )

import Language.Sunroof.JavaScript ( Expr, E(Var), Type(Base,Unit), literal )

-- -------------------------------------------------------------
-- UniqM Type Class
-- -------------------------------------------------------------

-- | Used for unique number generation.
type Uniq = Int

-- | Implemented if a monad supports unique number generation.
class Monad m => UniqM m where
  -- | Generate a unique number.
  uniqM :: m Uniq

-- | Creates a Javascript variable of any Sunroof type.
mkVar :: Sunroof a => Uniq -> a
mkVar = box . Var . ("v" ++) . show

-- | Create a unique Javascript variable of any Sunroof type.
jsVar :: (Sunroof a, UniqM m) => m a
jsVar = uniqM >>= return . mkVar

-- -------------------------------------------------------------
-- Sunroof Type Class
-- -------------------------------------------------------------

-- | Central type class of Sunroof. Every type that can be translated
--   into Javascript with @Sunroof@ has to implement this type class.
--   Everything that is a @Sunroof@ is also a @SunroofArgument@,
--   but not necessarily the other way round.
class SunroofArgument a => Sunroof a where
  -- | Create a Sunroof value from a plain Javascript expression.
  box :: Expr -> a
  -- | Reveal the plain Javascript expression that represents this Sunroof value.
  unbox :: a -> Expr
  -- | Returns the type of Javascript expression this Sunroof value
  --   represents. The default implementation returns 'Base' as type.
  typeOf :: Proxy a -> Type
  typeOf _ = Base

-- | Unit is a Sunroof value. It can be viewed as a representation
--   of @null@ or @void@.
instance Sunroof () where
--  showVar _ = ""
  box _ = ()
  unbox () = literal "null"
  typeOf _ = Unit

-- -------------------------------------------------------------
-- SunroofValue Type Class
-- -------------------------------------------------------------

-- | All Haskell values that can be converted into a 'Sunroof'
--   value in a pure way (no observable allocation) 
--   implement this class.
class SunroofValue a where
  -- | The Sunroot type that is equivalent to the implementing Haskell type.
  type ValueOf a :: *
  -- | Convert the Haskell value to its Sunroof equivalent.
  js :: (Sunroof (ValueOf a)) => a -> ValueOf a

-- | Unit is unit.
instance SunroofValue () where
  type ValueOf () = ()
  js () = ()

-- -------------------------------------------------------------
-- SunroofArgument Type Class
-- -------------------------------------------------------------

-- | Everything that can be used as argument to a function is Javascript/Sunroof.
class SunroofArgument args where
  -- | Turn the argument into a list of expressions.
  jsArgs   :: args -> [Expr]
  -- | Create a list of fresh variables for the arguments.
  jsValue  :: (UniqM m) => m args
  -- | Get the type of the argument values.
  typesOf  :: Proxy args -> [Type]

-- | Every 'Sunroof' value can be an argument to a function.
#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPABLE #-} Sunroof a => SunroofArgument a where
#else
instance Sunroof a => SunroofArgument a where
#endif
  jsArgs a = [unbox a]
  jsValue = jsVar
  typesOf p = [typeOf p]

-- | Unit is the empty argument list.
#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPPING #-} SunroofArgument () where
#else
instance SunroofArgument () where
#endif
  jsArgs _ = []
  jsValue = return ()
  typesOf _ = []

-- | Two arguments.
instance (Sunroof a, Sunroof b) => SunroofArgument (a,b) where
  jsArgs (a,b) = [unbox a, unbox b]
  jsValue = liftM2 (,) jsVar jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a),typeOf (Proxy :: Proxy b)]

-- | Three arguments.
instance (Sunroof a, Sunroof b, Sunroof c) => SunroofArgument (a,b,c) where
  jsArgs (a,b,c) = [unbox a, unbox b, unbox c]
  jsValue = liftM3 (,,) jsVar jsVar jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ]

-- | Four arguments.
instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d) => SunroofArgument (a,b,c,d) where
  jsArgs (a,b,c,d) = [unbox a, unbox b, unbox c, unbox d]
  jsValue = liftM4 (,,,) jsVar jsVar jsVar jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ]

-- | Five arguments.
instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e) => SunroofArgument (a,b,c,d,e) where
  jsArgs (a,b,c,d,e) = [unbox a, unbox b, unbox c, unbox d, unbox e]
  jsValue = liftM5 (,,,,) jsVar jsVar jsVar jsVar jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ,typeOf (Proxy :: Proxy e)
                  ]

-- | Six arguments.
instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f) => SunroofArgument (a,b,c,d,e,f) where
  jsArgs (a,b,c,d,e,f) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f]
  jsValue = return (,,,,,) `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ,typeOf (Proxy :: Proxy e)
                  ,typeOf (Proxy :: Proxy f)
                  ]

-- | Seven arguments.
instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f, Sunroof g) => SunroofArgument (a,b,c,d,e,f,g) where
  jsArgs (a,b,c,d,e,f,g) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f, unbox g]
  jsValue = return (,,,,,,) `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar
  typesOf Proxy = [typeOf (Proxy :: Proxy a)
                  ,typeOf (Proxy :: Proxy b)
                  ,typeOf (Proxy :: Proxy c)
                  ,typeOf (Proxy :: Proxy d)
                  ,typeOf (Proxy :: Proxy e)
                  ,typeOf (Proxy :: Proxy f)
                  ,typeOf (Proxy :: Proxy g)
                  ]

-- | Eight arguments.
instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f, Sunroof g, Sunroof h) => SunroofArgument (a,b,c,d,e,f,g,h) where
  jsArgs (a,b,c,d,e,f,g,h) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f, unbox g, unbox h]
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

-- | Nine arguments.
instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f, Sunroof g, Sunroof h, Sunroof i) => SunroofArgument (a,b,c,d,e,f,g,h,i) where
  jsArgs (a,b,c,d,e,f,g,h,i) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f, unbox g, unbox h, unbox i]
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






