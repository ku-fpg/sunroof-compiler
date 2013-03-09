{-# LANGUAGE OverloadedStrings, GADTs, MultiParamTypeClasses, ScopedTypeVariables, RankNTypes, DataKinds, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

-- This should only export user-facing types (as much as possible)
module Language.Sunroof.Types where

import Data.Monoid
import qualified Data.Semigroup as Semi
import Control.Monad.Operational
import Data.Boolean
import Control.Monad

import Language.Sunroof.JavaScript
import Language.Sunroof.Classes ( Sunroof(..), SunroofValue(..) )
import Language.Sunroof.Selector ( JSSelector, label, (!) )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Object ( JSObject )
import Language.Sunroof.JS.String ( string )
import Language.Sunroof.JS.Number ( JSNumber )

type Uniq = Int         -- used as a unique label

cast :: (Sunroof a, Sunroof b) => a -> b
cast = box . unbox

-- cast to int?
int :: (Sunroof a) => a -> JSNumber
int = box . (\ e -> uniOp "(int)" e) . unbox

mkVar :: Sunroof a => Uniq -> a
mkVar = box . Var . ("v" ++) . show


---------------------------------------------------------------

class Monad m => UniqM m where
        uniqM :: m Uniq

jsVar :: (Sunroof a, UniqM m) => m a
jsVar = uniqM >>= return . mkVar

---------------------------------------------------------------

class JSArgument args where
        jsArgs   :: args -> [Expr]        -- turn a value into a list of expressions
        jsValue  :: (UniqM m) => m args

instance Sunroof a => JSArgument a where
      jsArgs a = [unbox a]
      jsValue = jsVar

instance JSArgument () where
        jsArgs _ = []
        jsValue = return ()

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
    ifB = jsIfB

instance (JSArgument a, Sunroof b) => SunroofValue (a -> JS A b) where
  type ValueOf (a -> JS A b) = JS A (JSFunction a b)    -- TO revisit
  js = function

---------------------------------------------------------------

infix  5 :=

--type Action a r = a -> JS A r

---------------------------------------------------------------

-- | @invoke s a o@ calls the method with name @s@ using the arguments @a@
--   on the object @o@. A typical use would look like this:
--   
-- > o # invoke "foo" (x, y)
--   
--   Another use case is writing Javascript API bindings for common methods:
--   
-- > getElementById :: JSString -> JSObject -> JS t JSObject
-- > getElementById s = invoke "getElementById" s
--   
--   Like this the flexible type signature gets fixed.
invoke :: (JSArgument a, Sunroof r, Sunroof o) => String -> a -> o -> JS t r
invoke str args obj = (cast obj ! attribute str) `apply` args

object :: String -> JSObject
object = box . literal

-- perhaps call this invoke, or fun
-- SBC: fun
fun :: (JSArgument a, Sunroof r) => String -> JSFunction a r
fun = JSFunction . literal

-- TODO: BROKEN: Ignores the argument
-- Problem: new "Object" ()  -->  "(new Object)()" which will fail.
-- Should turn into "new Object()"
new :: (JSArgument a) => String -> a -> JS t JSObject
new cons _args = evaluate $ object $ "new " ++ cons ++ "()" --fun ("new " ++ cons) `apply` args

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
evaluate a  = single (JS_Eval a)

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
    (:=) :: (Sunroof a) => JSSelector a -> a -> JSObject                   -> JS t ()

-- replace calls to JS $ singleton with single
single :: JSI t a -> JS t a
single i = JS $ \ k -> singleton i >>= k

unJS :: JS t a -> (a -> Program (JSI t) ()) -> Program (JSI t) ()
unJS (JS m) k = m k
unJS ((:=) sel a obj) k = singleton (JS_Assign sel a obj) >>= k

instance Monad (JS t) where
        return a = JS $ \ k -> return a >>= k
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
function' = single . JS_Function

infixl 1 `apply`

-- | @apply f a@ applies the function @f@ to the given arguments @a@.
--   A typical use case looks like this:
--   
-- > foo `apply` (x,y)
--   
--   See '($$)' for a convenient infix operator to du this.
apply :: (JSArgument args, Sunroof ret) => JSFunction args ret -> args -> JS t ret
apply f args = f # with args
  where
    with :: (JSArgument a, Sunroof r) => a -> JSFunction a r -> JS t r
    with a fn = single $ JS_Invoke (jsArgs a) fn

-- | @f $$ a@ applies the function 'f' to the given arguments @a@.
--   See 'apply'.
($$) :: (JSArgument args, Sunroof ret) => JSFunction args ret -> args -> JS t ret
($$) = apply

infixr 0 #

-- We should use this operator for the obj.label concept.
-- It has been used in other places (but I can not seems
-- to find a library for it)
(#) :: a -> (a -> JS t b) -> JS t b
(#) obj act = act obj

type instance BooleanOf (JS t a) = JSBool

-- TODO: generalize
instance (JSThread t, Sunroof a, JSArgument a) => IfB (JS t a) where
    ifB i h e = single $ JS_Branch i h e

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
goto cont argument = JS $ \ _ -> cont argument

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
abortJS = JS $ \ _ -> return ()


-----------------------------------------------------------------
--Utilties for B

-- | This is the IORef of Sunroof.
newtype JSRef a = JSRef JSObject

newJSRef :: (Sunroof a) => a -> JS t (JSRef a)
newJSRef a = do
        obj <- new "Object" ()
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
nullJS = box $ literal "null"

-----------------------------------------------------------------
--
-- This is hacked right now
liftJS :: (Sunroof a) => JS A a -> JS t a
liftJS m = do
        o <- function (\ () -> m)
        apply o ()

-- If something is a JSTuple, then it can be passed (amoung other things)
-- as an argument by a javascript function.

class Sunroof o => JSTuple o where
        type Internals o
        match :: (Sunroof o) => o -> Internals o
        tuple :: Internals o -> JS t o

instance JSTuple JSObject where
        type Internals JSObject = ()
        match _ = ()
        tuple () = new "Object" ()

--------------------------------------------------------------------------------------





