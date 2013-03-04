{-# LANGUAGE OverloadedStrings, GADTs, MultiParamTypeClasses, ScopedTypeVariables, RankNTypes, DataKinds, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- This should only export user-facing types (as much as possible)
module Language.Sunroof.Types where

import Language.Sunroof.JavaScript

import Prelude hiding (div, mod, quot, rem, floor, ceiling, isNaN, isInfinite)
import GHC.Exts
--import Data.Char ( isDigit, isControl, isAscii, ord )
import Data.List ( intercalate )
--import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Semigroup as Semi
import Control.Monad.Operational
import Data.Boolean
import Data.Boolean.Numbers
import Control.Monad
import Data.Char
import Data.AdditiveGroup
import Data.VectorSpace hiding ((<.>))
import Numeric ( showHex )
import Data.Proxy
import Data.Reify
import Control.Applicative ( Applicative, pure, (<$>), (<*>))
import Data.Traversable
import Data.Foldable hiding (all, any)

type Uniq = Int         -- used as a unique label

cast :: (Sunroof a, Sunroof b) => a -> b
cast = box . unbox

-- cast to int?
int :: (Sunroof a) => a -> JSNumber
int = box . (\ e -> op "(int)" [ExprE e]) . unbox


litparen :: String -> String
litparen nm | all (\ c -> isDigit c || c == '.') nm = nm
            | otherwise      = "(" ++ nm ++ ")"

mkVar :: Sunroof a => Uniq -> a
mkVar = box . Var . ("v" ++) . show

class Show a => Sunroof a where
        box :: Expr -> a
        unbox :: a -> Expr

        showVar :: a -> String -- needed because show instance for unit is problematic
        showVar = show

        typeOf :: a -> Type
        typeOf _ = Base

-- unit is the oddball
instance Sunroof () where
--        showVar _ = ""
        box _ = ()
        unbox () = Lit "null"
        typeOf _ = Unit

---------------------------------------------------------------

class Monad m => UniqM m where
        uniqM :: m Uniq

jsVar :: (Sunroof a, UniqM m) => m a
jsVar = uniqM >>= return . mkVar

---------------------------------------------------------------

class SunroofValue a where
  type ValueOf a :: *
  js :: a -> ValueOf a

instance SunroofValue () where
  type ValueOf () = ()
  js () = ()

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

instance (JSArgument a, Sunroof b) => JSArgument (JSFunction a b) where
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

--op :: String -> [Expr] -> Expr
op n = Apply (ExprE $ Var n)

data JSBool = JSBool Expr

instance Show JSBool where
        show (JSBool e) = showExpr False e

instance Sunroof JSBool where
        box = JSBool
        unbox (JSBool v)  = v

instance Boolean JSBool where
  true          = JSBool (Lit "true")
  false         = JSBool (Lit "false")
  notB  (JSBool e1) = JSBool $ op "!" [ExprE e1]
  (&&*) (JSBool e1)
        (JSBool e2) = JSBool $ binOp "&&" e1 e2
  (||*) (JSBool e1)
        (JSBool e2) = JSBool $ binOp "||" e1 e2

type instance BooleanOf JSBool = JSBool

instance IfB JSBool where
    ifB = js_ifB

instance EqB JSBool where
  (==*) e1 e2 = JSBool $ binOp "==" (unbox e1) (unbox e2)
  (/=*) e1 e2 = JSBool $ binOp "!=" (unbox e1) (unbox e2)

js_ifB :: (Sunroof a) => JSBool -> a -> a -> a
js_ifB (JSBool c) t e = box $ op "?:" [ExprE c,ExprE $ unbox t,ExprE $ unbox e]

instance SunroofValue Bool where
  type ValueOf Bool = JSBool
  js True = true
  js False = false

---------------------------------------------------------------

-- The first type argument is the type of function argument;
-- The second type argument of JSFunction is what the function returns.
data JSFunction args ret = JSFunction Expr

instance Show (JSFunction a r) where
        show (JSFunction v) = showExpr False v

instance forall a r . (JSArgument a, Sunroof r) => Sunroof (JSFunction a r) where
        box = JSFunction
        unbox (JSFunction e) = e
        typeOf _ = Fun (length (jsArgs (error "instance Sunroof JSFunction" :: a)))

type instance BooleanOf (JSFunction a r) = JSBool

instance (JSArgument a, Sunroof r) => IfB (JSFunction a r) where
    ifB = js_ifB

instance (JSArgument a, Sunroof b) => SunroofValue (a -> JS A b) where
  type ValueOf (a -> JS A b) = JS A (JSFunction a b)    -- TO revisit
  js = function

---------------------------------------------------------------

binOp :: String -> Expr -> Expr -> E ExprE
binOp o e1 e2 = op o [ExprE e1, ExprE e2]

uniOp :: String -> Expr -> E ExprE
uniOp o e = op o [ExprE e]

---------------------------------------------------------------

data JSNumber = JSNumber Expr

instance Show JSNumber where
        show (JSNumber v) = showExpr False v

instance Sunroof JSNumber where
        box = JSNumber
        unbox (JSNumber e) = e

instance Num JSNumber where
        (JSNumber e1) + (JSNumber e2) = JSNumber $ binOp "+" e1 e2
        (JSNumber e1) - (JSNumber e2) = JSNumber $ binOp "-" e1 e2
        (JSNumber e1) * (JSNumber e2) = JSNumber $ binOp "*" e1 e2
        abs (JSNumber e1) = JSNumber $ uniOp "Math.abs" e1
        signum (JSNumber _e1) = error "signum" -- JSNumber $ uniOp "ERROR" e1
        fromInteger = JSNumber . Lit . litparen . show

instance IntegralB JSNumber where
  quot a b = ifB ((a / b) <* 0)
                 (JSNumber $ uniOp "Math.ceil" (let JSNumber res = a / b in res))
                 (a `div` b)
  rem a b = a - (a `quot` b)*b
  div a b = JSNumber $ uniOp "Math.floor" (let JSNumber res = a / b in res)
  mod (JSNumber a) (JSNumber b) = JSNumber $ binOp "%" a b


instance Fractional JSNumber where
        (JSNumber e1) / (JSNumber e2) = JSNumber $ binOp "/" e1 e2
        fromRational = JSNumber . Lit . litparen . show . (fromRational :: Rational -> Double)

instance Floating JSNumber where
        pi = JSNumber $ Lit $ "Math.PI"
        sin   (JSNumber e) = JSNumber $ uniOp "Math.sin"   e
        cos   (JSNumber e) = JSNumber $ uniOp "Math.cos"   e
        asin  (JSNumber e) = JSNumber $ uniOp "Math.asin"  e
        acos  (JSNumber e) = JSNumber $ uniOp "Math.acos"  e
        atan  (JSNumber e) = JSNumber $ uniOp "Math.atan"  e
        sinh  (JSNumber e) = JSNumber $ uniOp "Math.sinh"  e
        cosh  (JSNumber e) = JSNumber $ uniOp "Math.cosh"  e
        asinh (JSNumber e) = JSNumber $ uniOp "Math.asinh" e
        acosh (JSNumber e) = JSNumber $ uniOp "Math.acosh" e
        atanh (JSNumber e) = JSNumber $ uniOp "Math.atanh" e
        exp   (JSNumber e) = JSNumber $ uniOp "Math.exp"   e
        log   (JSNumber e) = JSNumber $ uniOp "Math.log"   e

instance RealFracB JSNumber where
  properFraction n =
    ( ifB (n >=* 0) (floor n) (ceiling n)
    , ifB (n >=* 0) (n - floor n) (n - ceiling n)
    )
  round   (JSNumber e) = JSNumber $ uniOp "Math.round" e
  ceiling (JSNumber e) = JSNumber $ uniOp "Math.ceil"  e
  floor   (JSNumber e) = JSNumber $ uniOp "Math.floor" e

instance RealFloatB JSNumber where
  isNaN (JSNumber a) = JSBool $ uniOp "isNaN" a
  isInfinite n = notB (isFinite n) &&* notB (isNaN n)
    where isFinite (JSNumber a) = JSBool $ uniOp "isFinite" a
  isNegativeZero n = isInfinite n &&* n <* 0
  isIEEE _ = true -- AFAIK
  atan2 (JSNumber a) (JSNumber b) = JSNumber $ op "Math.atan2" [ExprE a, ExprE b]

type instance BooleanOf JSNumber = JSBool

instance IfB JSNumber where
  ifB = js_ifB

instance EqB JSNumber where
  (==*) e1 e2 = JSBool $ binOp "==" (unbox e1) (unbox e2)
  (/=*) e1 e2 = JSBool $ binOp "!=" (unbox e1) (unbox e2)

instance OrdB JSNumber where
  (>*)  e1 e2 = JSBool $ binOp ">"  (unbox e1) (unbox e2)
  (>=*) e1 e2 = JSBool $ binOp ">=" (unbox e1) (unbox e2)
  (<*)  e1 e2 = JSBool $ binOp "<"  (unbox e1) (unbox e2)
  (<=*) e1 e2 = JSBool $ binOp "<=" (unbox e1) (unbox e2)

instance AdditiveGroup JSNumber where
        zeroV = 0
        (^+^) = (+)
        negateV = negate

instance VectorSpace JSNumber where
  type Scalar JSNumber = JSNumber
  s *^ d = s * d

instance SunroofValue Double where
  type ValueOf Double = JSNumber
  js = box . Lit . litparen . show

instance SunroofValue Float where
  type ValueOf Float = JSNumber
  js = box . Lit . litparen . show

instance SunroofValue Int where
  type ValueOf Int = JSNumber
  js = fromInteger . toInteger

instance SunroofValue Integer where
  type ValueOf Integer = JSNumber
  js = fromInteger . toInteger

instance SunroofValue Rational where
  type ValueOf Rational = JSNumber
  js = box . Lit . litparen . (show :: Double -> String) . fromRational

-- -------------------------------------------------------------
-- Javascript Strings
-- -------------------------------------------------------------

data JSString = JSString Expr

instance Show JSString where
        show (JSString v) = showExpr False v

instance Sunroof JSString where
        box = JSString
        unbox (JSString e) = e

instance Semi.Semigroup JSString where
        (JSString e1) <>(JSString e2) = JSString $ binOp "+" e1 e2


instance Monoid JSString where
        mempty = fromString ""
        mappend (JSString e1) (JSString e2) = JSString $ binOp "+" e1 e2

instance IsString JSString where
    fromString = JSString . Lit . jsLiteralString

type instance BooleanOf JSString = JSBool

instance IfB JSString where
    ifB = js_ifB

instance EqB JSString where
    (==*) e1 e2 = JSBool $ binOp "==" (unbox e1) (unbox e2)
    (/=*) e1 e2 = JSBool $ binOp "!=" (unbox e1) (unbox e2)

instance SunroofValue [Char] where
  type ValueOf [Char] = JSString
  js = fromString

instance SunroofValue Char where
  type ValueOf Char = JSString
  js c = fromString [c]

-- -------------------------------------------------------------
-- String Conversion Utilities: Haskell -> JS
-- -------------------------------------------------------------

-- | Transform a Haskell string into a string representing a JS string literal.
jsLiteralString :: String -> String
jsLiteralString = jsQuoteString . jsEscapeString

-- | Add quotes to a string.
jsQuoteString :: String -> String
jsQuoteString s = "\"" ++ s ++ "\""

-- | Transform a character to a string that represents its JS
--   unicode escape sequence.
jsUnicodeChar :: Char -> String
jsUnicodeChar c =
  let hex = showHex (ord c) ""
  in ('\\':'u': replicate (4 - length hex) '0') ++ hex

-- | Correctly replace Haskell characters by the JS escape sequences.
jsEscapeString :: String -> String
jsEscapeString [] = []
jsEscapeString (c:cs) = case c of
  -- Backslash has to remain backslash in JS.
  '\\' -> '\\' : '\\' : jsEscapeString cs
  -- Special control sequences.
  '\0' -> jsUnicodeChar '\0' ++ jsEscapeString cs -- Ambigous with numbers
  '\a' -> jsUnicodeChar '\a' ++ jsEscapeString cs -- Non JS
  '\b' -> '\\' : 'b' : jsEscapeString cs
  '\f' -> '\\' : 'f' : jsEscapeString cs
  '\n' -> '\\' : 'n' : jsEscapeString cs
  '\r' -> '\\' : 'r' : jsEscapeString cs
  '\t' -> '\\' : 't' : jsEscapeString cs
  '\v' -> '\\' : 'v' : jsEscapeString cs
  '\"' -> '\\' : '\"' : jsEscapeString cs
  '\'' -> '\\' : '\'' : jsEscapeString cs
  -- Non-control ASCII characters can remain as they are.
  c' | not (isControl c') && isAscii c' -> c' : jsEscapeString cs
  -- All other non ASCII signs are escaped to unicode.
  c' -> jsUnicodeChar c' ++ jsEscapeString cs

-- -------------------------------------------------------------
-- Javascript Objects
-- -------------------------------------------------------------

data JSObject = JSObject Expr

instance Show JSObject where
        show (JSObject v) = showExpr False v

instance Sunroof JSObject where
        box = JSObject
        unbox (JSObject o) = o

type instance BooleanOf JSObject = JSBool

instance IfB JSObject where
    ifB = js_ifB

instance SunroofValue Expr where
  type ValueOf Expr = JSObject
  js = box

-- -------------------------------------------------------------
-- Javascript Arrays
-- -------------------------------------------------------------

data JSArray a = JSArray Expr

instance Show (JSArray a) where
        show (JSArray v) = showExpr False v

instance (Sunroof a) => Sunroof (JSArray a) where
        box = JSArray
        unbox (JSArray o) = o

type instance BooleanOf (JSArray a) = JSBool

instance (Sunroof a) => IfB (JSArray a) where
    ifB = js_ifB
{- This conflicts with the string instance.
instance (SunroofValue a) => SunroofValue [a] where
  type ValueOf [a] = JSArray (ValueOf a)
  -- Uses JSON
  js l  = JSArray $ Lit $ "[" ++ intercalate "," (fmap (showVar . js) l) ++ "]"
-}

array :: (SunroofValue a, Sunroof (ValueOf a)) => [a] -> JSArray (ValueOf a)
array l  = JSArray $ Lit $ "[" ++ intercalate "," (fmap (showVar . js) l) ++ "]"

instance forall a . (Sunroof a) => Monoid (JSArray a) where
  mempty = emptyArray
  mappend (JSArray e1) (JSArray e2) = JSArray $ binOp "[].concat" e1 e2

-- Operations on arrays
newArray :: (Sunroof a) => JS t (JSArray a)
newArray = evaluate $ cast $ object "new Array()"

emptyArray :: (Sunroof a) => JSArray a
emptyArray = JSArray $ Lit "[]"

lengthArray :: (Sunroof a) => JSArray a -> JSNumber
lengthArray o = cast o ! "length"

pushArray :: (JSArgument a, Sunroof a) => a -> JSArray a -> JS t ()
pushArray a = method "push" a . cast

unshiftArray :: (JSArgument a, Sunroof a) => a -> JSArray a -> JS t ()
unshiftArray a = method "unshift" a . cast

popArray :: (Sunroof a) => JSArray a -> JS t a
popArray = method "pop" () . cast

shiftArray :: (Sunroof a) => JSArray a -> JS t a
shiftArray = method "shift" () . cast

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

infixl 1 !

(!) :: forall a . (Sunroof a) => JSObject -> JSSelector a -> a
(!) arr (JSSelector idx) = box $ Dot (ExprE $ unbox arr) (ExprE $ unbox idx) (typeOf (error "(!)" :: a))

---------------------------------------------------------------

infix  5 :=

type Action a r = a -> JS A r

---------------------------------------------------------------

-- TODO: not sure about the string => JSSelector (JSFunction a) overloading.
--method :: JSSelector (JSFunction a) -> [JSValue] -> Action JSObject a

-- SBC: call
method :: (JSArgument a, Sunroof r) => String -> a -> JSObject -> JS t r
method str args obj = (obj ! attribute str) `apply` args

string :: String -> JSString
string = fromString

object :: String -> JSObject
object = JSObject . Lit

-- perhaps call this invoke, or fun
-- SBC: fun
call :: String -> JSFunction a r
call = JSFunction . Lit

with :: (JSArgument a, Sunroof r) => a -> JSFunction a r -> JS t r
with a fn = JS_ $ singleton $ JS_Invoke (jsArgs a) fn

-- TODO: should take String argument
new :: JS t JSObject
new = evaluate $ object "new Object()"

attribute :: String -> JSSelector a
attribute attr = label $ string attr

--vector :: [JSValue] -> JSVector
--vector = ...
---------------------------------------------------------------

--select :: (Sunroof a) => JSSelector a -> JSObject -> JS a
--select sel obj = evaluate (obj ! sel) JS $ singleton $ JS_Select sel obj

---------------------------------------------------------------

-- This is not the same as return; it evaluates
-- the argument to value form.
evaluate, value :: (Sunroof a) => a -> JS t a
evaluate a  = JS_ $ singleton (JS_Eval a)

value = evaluate

---------------------------------------------------------------

--data A = AX  -- Atomic
--data B = BX  -- Blocking

data T = A | B
        deriving (Eq, Ord, Show)

data ThreadProxy (t :: T) = ThreadProxy

class JSThreadReturn t () => JSThread (t :: T) where
    evalStyle    :: ThreadProxy t -> T

--class JSThreadReturn t () => JSThread' (t :: T) where
--    evalStyle'    :: ThreadProxy t -> T

instance JSThread A where
    evalStyle _ = A

instance JSThread B where
    evalStyle _ = B

class (Sunroof a) => JSThreadReturn (t :: T) a where
    threadCloser :: a -> Program (JSI t) ()

instance (Sunroof a) => JSThreadReturn A a where
    threadCloser = singleton . JS_Return

instance JSThreadReturn B () where
    threadCloser () = return ()



-- Control.Monad.Operational makes a monad out of JS for us
data JS :: T -> * -> * where

    JS   :: ((a -> Program (JSI t) ()) -> Program (JSI t) ())              -> JS t a            -- TO CALL JSB
    JS_    :: Program (JSI t) a                                            -> JS t a            -- TO CALL JSA

--    JSA   :: Program (JSI t) a                                             -> JS t a
--    JSB   :: ((a -> Program (JSI t) ()) -> Program (JSI t) ())             -> JS t B

    (:=) :: (Sunroof a) => JSSelector a -> a -> JSObject                   -> JS t ()

-- replace calls to JS $ singleton with single
single :: JSI t a -> JS t a
single i = JS_ $ singleton i

unJS :: JS t a -> (a -> Program (JSI t) ()) -> Program (JSI t) ()
unJS (JS m) k = m k
unJS (JS_ m) k = m >>= k
unJS ((:=) sel a obj) k = singleton (JS_Assign sel a obj) >>= k

instance Monad (JS t) where
        return a = JS_ $ return a       -- preserves the threadary kind
        m >>= k = JS $ \ k0 -> unJS m (\ r -> unJS (k r) k0)

-- | We define the Semigroup instance for JS, where
--  the first result (but not the first effect) is discarded.
--  Thus, '<>' is the analog of the monadic '>>'.
instance Semi.Semigroup (JS t a) where
        js1 <> js2 = js1 >> js2

instance Monoid (JS t ()) where
        mempty = return ()
        mappend = (<>)

-- define primitive effects / "instructions" for the JS monad
data JSI :: T -> * -> * where

    -- apply an action to an 'a', and compute a b
--    JS_App    :: (Sunroof a, Sunroof b) => a -> Action a b      -> JSI b

    JS_Assign  :: (Sunroof a) => JSSelector a -> a -> JSObject  -> JSI t ()

    JS_Select  :: (Sunroof a) => JSSelector a -> JSObject      -> JSI t a

    JS_Invoke :: (JSArgument a, Sunroof r) => [Expr] -> JSFunction a r  -> JSI t r        -- Perhaps take the overloaded vs [Expr]
                                                                                -- and use jsArgs in the compiler?
    -- Not the same as return; does evaluation of argument
    JS_Eval   :: (Sunroof a) => a                                       -> JSI t a

    JS_Function :: (JSThreadReturn t2 b, JSArgument a, Sunroof b) => (a -> JS t2 b) -> JSI t (JSFunction a b)
    -- Needs? Boolean bool, bool ~ BooleanOf (JS a)
    JS_Branch :: (JSThread t, Sunroof a, JSArgument a, Sunroof bool) => bool -> JS t a -> JS t a  -> JSI t a
    -- A loop primitive.
    JS_Foreach :: (Sunroof a, Sunroof b) => JSArray a -> (a -> JS A b)  -> JSI A ()        -- to visit / generalize later

    -- syntaxtical return in javascript; only used in code generator for now.
    JS_Return  :: (Sunroof a) => a                                      -> JSI t ()      -- literal return
    JS_Assign_ :: (Sunroof a) => Id -> a                                -> JSI t ()     -- a classical effect
                        -- TODO: generalize Assign[_] to have a RHS

---------------------------------------------------------------

-- | We can compile A-tomic functions.

function :: (JSArgument a, Sunroof b) => (a -> JS A b) -> JS t (JSFunction a b)
function = function'

-- | We can compile B-lockable functions that return ().
-- Note that, with the 'B'-style threads, we return from a call at the first block,
-- not at completion of the call.

continuation :: (JSArgument a) => (a -> JS B ()) -> JS t (JSFunction a ())
continuation = function'

-- The generalization of function and continuation.

function' :: (JSThreadReturn t2 b, JSArgument a, Sunroof b) => (a -> JS t2 b) -> JS t (JSFunction a b)
function' = JS_ . singleton . JS_Function

infixl 1 `apply`

-- | Call a function with the given arguments.
apply :: (JSArgument args, Sunroof ret) => JSFunction args ret -> args -> JS t ret
apply f args = f # with args

foreach :: (Sunroof a, Sunroof b) => JSArray a -> (a -> JS A b) -> JS A ()
foreach arr body = JS_ $ singleton $ JS_Foreach arr body

infixl 1 #

-- We should use this operator for the obj.label concept.
-- It has been used in other places (but I can not seems
-- to find a library for it)
(#) :: a -> (a -> JS t b) -> JS t b
(#) obj act = act obj

type instance BooleanOf (JS t a) = JSBool

-- TODO: generalize
instance (JSThread t, Sunroof a, JSArgument a) => IfB (JS t a) where
    ifB i h e = JS_ $ singleton $ JS_Branch i h e

switch :: (EqB a, BooleanOf a ~ JSBool, Sunroof a, Sunroof b, JSArgument b, JSThread t) => a -> [(a,JS t b)] -> JS t b
switch _a [] = return (cast (object "undefined"))
switch a ((c,t):e) = ifB (a ==* c) t (switch a e)

---------------------------------------------------------------
{-
-- The JS (Blocking) is continuation based.
newtype JSB a = JSB { unJSB :: (a -> JS A ()) -> JS A () }

instance Monad JSB where
        return a = JSB $ \ k -> k a
        (JSB m) >>= k = JSB $ \ k0 -> m (\ a -> unJSB (k a) k0)
-}

type JSA a = JS A a
type JSB a = JS B a

---------------------------------------------------------------

         --  ((a -> M ()) -> M ()) -> JS t a
--continue :: ((forall a . (JSArgument a) => a -> JS B ()) -> JS B ()) -> JS B ()
--continue f = JS $ \ k -> down (undefined) -- f (down . k))

-- Implementation of goto and callCC from
--   http://stackoverflow.com/questions/9050725/call-cc-implementation
--
goto :: (x ~ ()) => (a -> Program (JSI B) ()) -> a -> JS B x
goto continuation argument = JS $ \ _ -> continuation argument

--callCC :: ((a -> JS 'B x) -> JS 'B a) -> JS 'B a
callcc :: (x ~ ()) => ((a -> JS 'B x) -> JS 'B a) -> JS 'B a
callcc f = JS $ \ cc -> unJS (f (goto cc)) cc

-- this one discards its
callcc' :: ((a -> JS 'B ()) -> JS 'B ()) -> JS 'B a
callcc' f = JS $ \ cc -> unJS (f (goto cc)) return

-- | reify the current contination as a JavaScript function.
-- unlike callcc, captures then discards the continuation.

reifyccJS :: JSArgument a => (JSFunction a () -> JS B ()) -> JS B a
reifyccJS f = JS $ \ cc -> unJS (do o <- continuation (goto cc)
                                    f o
                               ) (\ _ -> return ())


abortJS :: JS B a
abortJS = JS $ \ cc -> return ()


-----------------------------------------------------------------
--Utilties for B

-- | This is the IORef of Sunroof.
newtype JSRef a = JSRef JSObject

newJSRef :: (Sunroof a) => a -> JS t (JSRef a)
newJSRef a = do
        obj <- new
        obj # "val" := a
        return $ JSRef obj

-- | This a a non-blocking read
readJSRef :: (Sunroof a) => JSRef a -> JS t a
readJSRef (JSRef obj) = evaluate $ obj ! "val"

-- | This a a non-blocking write
writeJSRef :: (Sunroof a) => JSRef a -> a -> JS t ()
writeJSRef (JSRef obj) a = obj # "val" := a

modifyJSRef :: (Sunroof a) => JSRef a -> (a -> JS A a) -> JS A ()
modifyJSRef ref f = do
        val <- readJSRef ref
        f val >>= writeJSRef ref

-----------------------------------------------------------------
--

nullJS :: JSObject
nullJS = box $ Lit "null"

-----------------------------------------------------------------
--
-- This is hacked right now
liftJS :: (Sunroof a) => JS A a -> JS t a
liftJS m = do
        o <- function (\ () -> m)
        apply o ()

