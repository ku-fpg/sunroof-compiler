 {-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances, TypeFamilies, UndecidableInstances #-}

module Language.Sunroof.Types where

import Prelude hiding (div, mod, quot, rem, floor, ceiling, isNaN, isInfinite)
import GHC.Exts
import Data.Char
import Data.List (intercalate)
--import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.Operational
import Data.Boolean
import Data.Boolean.Numbers
import Control.Monad

type Uniq = Int         -- used as a unique label

cast :: (Sunroof a, Sunroof b) => a -> b
cast = box . unbox

---------------------------------------------------------------
-- Trivial expression language
data Expr
        = Lit String    -- a precompiled version of this literal
        | Var Uniq
        | Op String [Expr]
--        | Cast JSValue

instance Show Expr where
        show (Lit a)  = a
        show (Var uq) = "v" ++ show uq
        show (Op "[]" [a,x]) = "(" ++ show a ++ ")[" ++ show x ++ "]"
        show (Op "?:" [a,x,y]) = "((" ++ show a ++ ")?(" ++ show x ++ "):(" ++ show y ++ "))"
--        show (Op "(,)" [x,y]) = "[" ++ show x ++ "," ++ show y ++ "]"
        show (Op x [a,b]) | all (not . isAlpha) x = "(" ++ show a ++ ")" ++ x ++ "(" ++ show b ++ ")"
        show (Op fn args) = fn ++ "(" ++ intercalate "," (map show args) ++ ")"
--        show (Cast e) = show e

---------------------------------------------------------------

mkVar :: Sunroof a => Uniq -> a
mkVar = box . Var

class Show a => Sunroof a where
        box :: Expr -> a
        unbox :: a -> Expr

        showVar :: a -> String -- needed because show instance for unit is problematic
        showVar = show

        assignVar :: a -> String -> String
        assignVar a rhs = "var " ++ show a ++ "=" ++ rhs ++ ";"

-- unit is the oddball
instance Sunroof () where
        showVar _ = ""
        assignVar _ rhs = rhs ++ ";"
        box _ = ()
        unbox () = Lit ""

---------------------------------------------------------------

class Monad m => UniqM m where
        uniqM :: m Uniq

jsVar :: (Sunroof a, UniqM m) => m a
jsVar = uniqM >>= return . mkVar

---------------------------------------------------------------

class JSArgument args where
        jsArgs   :: args -> [Expr]        -- turn a value into a list of expressions
        jsValue  :: (UniqM m) => m args

instance JSArgument () where
        jsArgs _ = []
        jsValue = return ()

instance JSArgument JSBool where
      jsArgs a = [unbox a]
      jsValue = jsVar

instance JSArgument JSNumber where
      jsArgs a = [unbox a]
      jsValue = jsVar

instance JSArgument JSString where
      jsArgs a = [unbox a]
      jsValue = jsVar

instance JSArgument JSObject where
      jsArgs a = [unbox a]
      jsValue = jsVar

instance (Sunroof a, Sunroof b) => JSArgument (a,b) where
      jsArgs ~(a,b) = [unbox a, unbox b]
      jsValue = liftM2 (,) jsVar jsVar

instance (Sunroof a, Sunroof b, Sunroof c) => JSArgument (a,b,c) where
      jsArgs ~(a,b,c) = [unbox a, unbox b, unbox c]
      jsValue = liftM3 (,,) jsVar jsVar jsVar

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d) => JSArgument (a,b,c,d) where
      jsArgs ~(a,b,c,d) = [unbox a, unbox b, unbox c, unbox d]
      jsValue = liftM4 (,,,) jsVar jsVar jsVar jsVar

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e) => JSArgument (a,b,c,d,e) where
      jsArgs ~(a,b,c,d,e) = [unbox a, unbox b, unbox c, unbox d, unbox e]
      jsValue = liftM5 (,,,,) jsVar jsVar jsVar jsVar jsVar

instance (Sunroof a, Sunroof b, Sunroof c, Sunroof d, Sunroof e, Sunroof f) => JSArgument (a,b,c,d,e,f) where
      jsArgs ~(a,b,c,d,e,f) = [unbox a, unbox b, unbox c, unbox d, unbox e, unbox f]
      jsValue = return (,,,,,) `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar `ap` jsVar

-- Need to add the 7 & 8 tuple (not used in this package - yet)

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

----    jsValue :: val -> JSValue

--instance JSType JSBool where
--    jsValue (JSBool expr) = JSValue expr

--instance JSPrimitive a => JSArgument a where

---------------------------------------------------------------

data JSBool = JSBool Expr

instance Show JSBool where
        show (JSBool e) = show e

instance Sunroof JSBool where
        box = JSBool
        unbox (JSBool v)  = v

--true = JSBool (Lit "true")
--false = JSBool (Lit "false")

instance Boolean JSBool where
  true          = JSBool (Lit "true")
  false         = JSBool (Lit "false")
  notB  (JSBool e1) = JSBool $ Op "!" [e1]
  (&&*) (JSBool e1)
        (JSBool e2) = JSBool $ Op "&&" [e1,e2]
  (||*) (JSBool e1)
        (JSBool e2) = JSBool $ Op "||" [e1,e2]

type instance BooleanOf JSBool = JSBool

instance IfB JSBool where
    ifB = js_ifB

instance EqB JSBool where
  (==*) e1 e2 = JSBool $ Op "==" [unbox e1,unbox e2]
  (/=*) e1 e2 = JSBool $ Op "!=" [unbox e1,unbox e2]

js_ifB :: (Sunroof a) => JSBool -> a -> a -> a
js_ifB (JSBool c) t e = box $ Op "?:" [c,unbox t,unbox e]

---------------------------------------------------------------

-- The first type argument is the type of function argument;
-- The second type argument of JSFunction is what the function returns.
data JSFunction args ret = JSFunction Expr

instance Show (JSFunction a r) where
        show (JSFunction v) = show v

instance forall a r . (JSArgument a) => Sunroof (JSFunction a r) where
        box = JSFunction
        unbox (JSFunction e) = e


--        assignVar :: a -> String -> String
        assignVar a rhs = "var " ++ show a ++ "= function(" ++ args ++ "){ return (" ++ rhs ++ "(" ++ args ++ "));};"
           where args = intercalate ","
                         [ "a" ++ show (i :: Int)
                         | (i,_) <- zip [1..] (jsArgs (error "" :: a))
                         ]


type instance BooleanOf (JSFunction a r) = JSBool

instance (JSArgument a) => IfB (JSFunction a r) where
    ifB = js_ifB

---------------------------------------------------------------

data JSNumber = JSNumber Expr

instance Show JSNumber where
        show (JSNumber v) = show v

instance Sunroof JSNumber where
        box = JSNumber
        unbox (JSNumber e) = e

instance Num JSNumber where
        (JSNumber e1) + (JSNumber e2) = JSNumber $ Op "+" [e1,e2]
        (JSNumber e1) - (JSNumber e2) = JSNumber $ Op "-" [e1,e2]
        (JSNumber e1) * (JSNumber e2) = JSNumber $ Op "*" [e1,e2]
        abs (JSNumber e1) = JSNumber $ Op "Math.abs" [e1]
        signum (JSNumber e1) = JSNumber $ Op "" [e1] -- TODO
        fromInteger = JSNumber . Lit . show

instance IntegralB JSNumber where
  quot a b = ifB ((a / b) <* 0)
                 (JSNumber $ Op "Math.ceil" [let JSNumber res = a / b in res])
                 (a `div` b)
  rem a b = a - (a `quot` b)*b
  div a b = JSNumber $ Op "Math.floor" [let JSNumber res = a / b in res]
  mod (JSNumber a) (JSNumber b) = JSNumber $ Op "%" [a, b]

instance Fractional JSNumber where
        (JSNumber e1) / (JSNumber e2) = JSNumber $ Op "/" [e1,e2]
        fromRational = JSNumber . Lit . show . (fromRational :: Rational -> Double)

instance Floating JSNumber where
        pi = JSNumber $ Lit $ "Math.PI"
        sin   (JSNumber e) = JSNumber $ Op "Math.sin"   [e]
        cos   (JSNumber e) = JSNumber $ Op "Math.cos"   [e]
        asin  (JSNumber e) = JSNumber $ Op "Math.asin"  [e]
        acos  (JSNumber e) = JSNumber $ Op "Math.acos"  [e]
        atan  (JSNumber e) = JSNumber $ Op "Math.atan"  [e]
        sinh  (JSNumber e) = JSNumber $ Op "Math.sinh"  [e]
        cosh  (JSNumber e) = JSNumber $ Op "Math.cosh"  [e]
        asinh (JSNumber e) = JSNumber $ Op "Math.asinh" [e]
        acosh (JSNumber e) = JSNumber $ Op "Math.acosh" [e]
        atanh (JSNumber e) = JSNumber $ Op "Math.atanh" [e]
        exp   (JSNumber e) = JSNumber $ Op "Math.exp"   [e]
        log   (JSNumber e) = JSNumber $ Op "Math.log"   [e]

instance RealFracB JSNumber where
  properFraction n =
    ( ifB (n >=* 0) (floor n) (ceiling n)
    , ifB (n >=* 0) (n - floor n) (n - ceiling n)
    )
  round   (JSNumber e) = JSNumber $ Op "Math.round" [e]
  ceiling (JSNumber e) = JSNumber $ Op "Math.ceil"  [e]
  floor   (JSNumber e) = JSNumber $ Op "Math.floor" [e]

instance RealFloatB JSNumber where
  isNaN (JSNumber a) = JSBool $ Op "isNaN" [a]
  isInfinite n = notB (isFinite n) &&* notB (isNaN n)
    where isFinite (JSNumber a) = JSBool $ Op "isFinite" [a]
  isNegativeZero n = isInfinite n &&* n <* 0
  isIEEE _ = true -- AFAIK
  atan2 (JSNumber a) (JSNumber b) = JSNumber $ Op "Math.atan2" [a, b]

type instance BooleanOf JSNumber = JSBool

instance IfB JSNumber where
  ifB = js_ifB

instance EqB JSNumber where
  (==*) e1 e2 = JSBool $ Op "==" [unbox e1,unbox e2]
  (/=*) e1 e2 = JSBool $ Op "!=" [unbox e1,unbox e2]

instance OrdB JSNumber where
  (>*)  e1 e2 = JSBool $ Op ">"  [unbox e1,unbox e2]
  (>=*) e1 e2 = JSBool $ Op ">=" [unbox e1,unbox e2]
  (<*)  e1 e2 = JSBool $ Op "<"  [unbox e1,unbox e2]
  (<=*) e1 e2 = JSBool $ Op "<=" [unbox e1,unbox e2]

---------------------------------------------------------------

data JSString = JSString Expr

instance Show JSString where
        show (JSString v) = show v

instance Sunroof JSString where
        box = JSString
        unbox (JSString e) = e

instance Monoid JSString where
        mempty = fromString ""
        mappend (JSString e1) (JSString e2) = JSString $ Op "+" [e1,e2]

instance IsString JSString where
    fromString = JSString . Lit . show

type instance BooleanOf JSString = JSBool

instance IfB JSString where
    ifB = js_ifB

instance EqB JSString where
    (==*) e1 e2 = JSBool $ Op "==" [unbox e1,unbox e2]
    (/=*) e1 e2 = JSBool $ Op "!=" [unbox e1,unbox e2]

---------------------------------------------------------------

data JSObject = JSObject Expr

instance Show JSObject where
        show (JSObject v) = show v

instance Sunroof JSObject where
        box = JSObject
        unbox (JSObject o) = o

type instance BooleanOf JSObject = JSBool

instance IfB JSObject where
    ifB = js_ifB

---------------------------------------------------------------

-- | a 'JSSelector' selects a field from a JSObject.
-- The phantom is the type of the selected value.
data JSSelector :: * -> * where
        JSSelector :: JSString           -> JSSelector a

instance IsString (JSSelector a) where
    fromString = JSSelector . fromString

instance Show (JSSelector a) where
    show (JSSelector str) = show str


label :: JSString -> JSSelector a
label = JSSelector

---------------------------------------------------------------

(!) :: forall a . (Sunroof a) => JSObject -> JSSelector a -> a
(!) arr (JSSelector idx) = box $ Op "[]" [unbox arr,unbox idx]

---------------------------------------------------------------

infix  5 :=


type Action a r = a -> JS r
{-
data Action :: * -> * -> * where
   -- Invoke is not quite right
   Invoke :: (Sunroof r) => [Expr]                              -> Action (JSFunction a r) r
   -- Basically, this is special form of call, to assign to a field
   (:=)   :: (Sunroof a) => JSSelector a -> a                   -> Action JSObject ()
   -- This is the fmap-like function, an effect-free modifier on the first argument
   -- TODO: revisit Map and Invoke. Does not feel right.
   Map :: (Sunroof b, a ~ JSObject, b ~ JSFunction x y, Sunroof c) => (a -> b) -> Action b c                 -> Action a c
   Field :: JSSelector a                                        -> Action JSObject a

   Dot :: Action a b -> Action b c                              -> Action a c

   NoAction :: b                                                -> Action a b
   BindAction :: Action a b -> (b -> Action a c)                -> Action a c

-- There is
--  Action JSObject a                           -- Return a             JSObject -> JS a
--  Action (JSFunction a r) r                   -- Function a r         JSFunction a r -> a -> JS r
-}
-- method :: String             -> a -> JSObject -> JS r
-- with :: a                    -> JSFunction a r -> JS r
-- (:=) :: JSSelector a -> a    -> JSObject -> JS ()

-- type Action a = JSObject -> JS a     ??? But we have the reader monad built in anyway???

-- (a -> JS b) -> (b -> JS c)
-- method :: String -> a -> Return r
-- with :: Return (JSFunction a b) -> a -> Return a
{-
instance Monad (Action a) where
        return = NoAction
        (>>=)  = BindAction
-}
---------------------------------------------------------------

-- TODO: not sure about the string => JSSelector (JSFunction a) overloading.
--method :: JSSelector (JSFunction a) -> [JSValue] -> Action JSObject a

method :: (JSArgument a, Sunroof r) => String -> a -> Action JSObject r
method str args obj = select (attribute str) obj >>= with args

string :: String -> JSString
string = JSString . Lit . show

object :: String -> JSObject
object = JSObject . Lit

-- perhaps call this invoke
call :: String -> JSFunction a r
call = JSFunction . Lit

with :: (JSArgument a, Sunroof r) => a -> Action (JSFunction a r) r
with a fn = JS $ singleton $ JS_Invoke (jsArgs a) fn

new :: JS JSObject
new = evaluate $ object "new Object()"

attribute :: String -> JSSelector a
attribute attr = label $ string attr

--vector :: [JSValue] -> JSVector
--vector = ...
---------------------------------------------------------------

select :: (Sunroof a) => JSSelector a -> JSObject -> JS a
select sel obj = JS $ singleton $ JS_Select sel obj

---------------------------------------------------------------

-- This is not the same as return; it evaluates
-- the argument to value form.
evaluate :: (Sunroof a) => a -> JS a
evaluate a  = JS $ singleton (JS_Eval a)

---------------------------------------------------------------

-- Control.Monad.Operational makes a monad out of JS for us
data JS a where
    JS   :: Program JSI a                                             -> JS a
    (:=) :: (Sunroof a) => JSSelector a -> a -> JSObject              -> JS ()

unJS :: JS a -> Program JSI a
unJS (JS m) = m
unJS ((:=) sel a obj) = singleton $ JS_Assign sel a obj

instance Monad JS where
        return a = JS (return a)
        m >>= k = JS (unJS m >>= \ r -> unJS (k r))

-- define primitive effects / "instructions" for the JS monad
data JSI a where

    -- apply an action to an 'a', and compute a b
--    JS_App    :: (Sunroof a, Sunroof b) => a -> Action a b      -> JSI b

    JS_Assign  :: (Sunroof a) => JSSelector a -> a -> JSObject  -> JSI ()

    JS_Select  :: (Sunroof a) => JSSelector a -> JSObject      -> JSI a

    JS_Invoke :: (JSArgument a, Sunroof r) => [Expr] -> JSFunction a r        -> JSI r        -- Perhaps take the overloaded vs [Expr]
                                                                                -- and use jsArgs in the compiler?

    -- Not the same as return; does evaluation of argument
    JS_Eval   :: (Sunroof a) => a                               -> JSI a

    JS_Function :: (JSArgument a, Sunroof b) => (a -> JS b)        -> JSI (JSFunction a b)
    -- Needs? Boolean bool, bool ~ BooleanOf (JS a)
    JS_Branch :: (Sunroof a, Sunroof bool) => bool -> JS a -> JS a -> JSI a

---------------------------------------------------------------

-- We only can compile functions that do not have interesting return
-- values, so we can assume they are continuation-like things.
function :: (JSArgument a, Sunroof b) => (a -> JS b) -> JS (JSFunction a b)
function = JS . singleton . JS_Function

-- | Call a function with the given arguments.
apply :: (JSArgument args, Sunroof ret) => JSFunction args ret -> args -> JS ret
apply f args = f # with args

(<!>) :: (Sunroof b) => JSObject -> JSSelector b -> JS b
(<!>) o s = evaluate $ o ! s

infixl 2 #

-- We should use this operator for the obj.label concept.
-- It has been used in other places (but I can not seems
-- to find a library for it)
(#) :: a -> (a -> JS b) -> JS b
(#) obj act = act obj

type instance BooleanOf (JS a) = JSBool

instance forall a . (Sunroof a) => IfB (JS a) where
    ifB i h e = JS $ singleton $ JS_Branch i h e

switch :: (EqB a, BooleanOf a ~ JSBool, Sunroof a, Sunroof b) => a -> [(a,JS b)] -> JS b
switch _a [] = return (cast (object "undefined"))
switch a ((c,t):e) = ifB (a ==* c) t (switch a e)


---------------------------------------------------------------

