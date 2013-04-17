
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

-- | The basic types and combinators of Sunroof.
module Language.Sunroof.Types
  ( T(..)
  , ThreadProxy(..)
  , SunroofThread(..)
  , JS(..), JSA, JSB
  , unJS
  , single
  , JSI(..)
  , callcc
  , done, liftJS, kast
  , JSFunction, JSContinuation
  , function , continuation, goto
  , apply, ($$)
  , cast
  , (#)
  , attr
  , fun, invoke, new
  , evaluate, value
  , switch
  , nullJS
  , delete
  , JSTuple(..)  -- TODO: Call this SunroofTuple?
  , SunroofKey(..)
  , SunroofFunctor(..)
  ) where

import Control.Monad.Operational

import Data.Monoid ( Monoid(..) )
--import Data.Semigroup ( Semigroup(..) )
import Data.Boolean ( BooleanOf, IfB(..), EqB(..) )
import Data.Proxy ( Proxy(Proxy) )
import Data.Semigroup ( Semigroup(..) )

import Language.Sunroof.JavaScript
  ( Expr, Type(Fun), Id
  , showExpr, literal )
import Language.Sunroof.Classes
  ( Sunroof(..), SunroofValue(..), SunroofArgument(..) )
import Language.Sunroof.Selector ( JSSelector, label, index, (!) )
import Language.Sunroof.JS.Bool ( JSBool, jsIfB )
import Language.Sunroof.JS.Object ( JSObject, object )
import Language.Sunroof.JS.Number ( JSNumber )
import Language.Sunroof.JS.String ( string, JSString )

-- -------------------------------------------------------------
-- Thread Model
-- -------------------------------------------------------------

-- | The possible threading models for Javascript computations.
data T = A -- ^ Atomic - The computation will not be interrupted.
       | B -- ^ Blocking - The computation may block and wait to enable
           --   interleaving with other computations.
       deriving (Eq, Ord, Show)

-- | A proxy to capture the type of threading model used.
--   See 'SunroofThread'.
data ThreadProxy (t :: T) = ThreadProxy

-- | When implemented the type supports determining the threading model
--   during runtime.
class SunroofThread (t :: T) where
  -- | Determine the used threading model captured the given 'ThreadProxy'
  --   object.
  evalStyle    :: ThreadProxy t -> T
  -- | Create a possibly blocking computation from the given one.
  blockableJS :: (Sunroof a) => JS t a -> JS B a

instance SunroofThread A where
  evalStyle _ = A
  blockableJS = liftJS

instance SunroofThread B where
  evalStyle _ = B
  blockableJS = id

-- -------------------------------------------------------------
-- JS Monad - The Javascript Monad
-- -------------------------------------------------------------

infix  5 :=

-- | The monadic type of Javascript computations.
--
--   @JS t a@ is a computation using the thread model @t@ (see 'T').
--   It returns a result of type @a@.
data JS :: T -> * -> * where
  JS   :: ((a -> Program (JSI t) ()) -> Program (JSI t) ()) -> JS t a
  (:=) :: (Sunroof a, Sunroof o) => JSSelector a -> a -> o -> JS t ()

-- | Short-hand type for atmoic Javascript computations.
type JSA a = JS A a

-- | Short-hand type for possibly blocking Javascript computations.
type JSB a = JS B a

-- | Lifts a single primitive Javascript instruction ('JSI') into the
--   'JS' monad.
single :: JSI t a -> JS t a
single i = JS $ \ k -> singleton i >>= k

-- | Unwraps the 'JS' monad into a continuation
--   on 'Control.Monad.Operational.Program'.
unJS :: JS t a -> (a -> Program (JSI t) ()) -> Program (JSI t) ()
unJS (JS m) k = m k
unJS ((:=) sel a obj) k = singleton (JS_Assign sel a (cast obj)) >>= k

instance Monad (JS t) where
  return a = JS $ \ k -> return a >>= k
  m >>= k = JS $ \ k0 -> unJS m (\ r -> unJS (k r) k0)

instance Functor (JS t) where
  fmap f jsm = jsm >>= (return . f)

type instance BooleanOf (JS t a) = JSBool

instance (SunroofThread t, SunroofArgument a) => IfB (JS t a) where
    ifB i h e = single $ JS_Branch i h e

-- | We define the Semigroup instance for JS, where
--   the first result (but not the first effect) is discarded.
--   Thus, '<>' is the analog of the monadic '>>'.
instance Semigroup (JS t a) where
  js1 <> js2 = js1 >> js2

instance Monoid (JS t ()) where
  mempty = return ()
  mappend = (<>)

-- | 'JSI' represents the primitive effects or instructions for
--   the JS monad.
--
--     [@JS_Assign s v o@] assigns a value @v@ to the selected field @s@
--       in the object @o@.
--
--     [@JS_Select s o@] returns the value of the selected field @s@
--       in the object @o@.
--
--     [@JS_Delete s o@] delete the selected field @s@ in the object @o@.
--
--     [@JS_Invoke a f@] calls the function @f@ with the arguments @a@.
--
--     [@JS_Eval v@] evaluates the value @v@. Subsequent instructions
--       use the value instead of reevaluating the expression.
--
--     [@JS_Function f@] creates a Javascript function
--       from the Haskell function @f@.
--
--     [@JS_Continuation f@] creates a Javascript continuation (function that never returns a value)
--       from the Haskell function @f@.
--
--     [@JS_Branch b t f@] creates a @if-then-else@ statement in Javascript.
--       In that statement @b@ is the condition, @t@ is the true branch and
--       @f@ is the false branch.
--
--     [@JS_Return v@] translates into an actual @return@ statement that
--       returns the value @v@ in Javascript.
--
--     [@JS_Assign_ v x@] assigns the value @x@ to the variable with name @v@.
--
--     [@JS_Fix v x@] models a fixpoint computation in 'JS'. See 'jsfix'.
--
data JSI :: T -> * -> * where
  JS_Assign  :: (Sunroof a) => JSSelector a -> a -> JSObject -> JSI t ()
  JS_Select  :: (Sunroof a) => JSSelector a -> JSObject -> JSI t a
  JS_Delete  :: (Sunroof a) => JSSelector a -> JSObject -> JSI t ()
  -- Perhaps take the overloaded vs [Expr] and use jsArgs in the compiler?
  JS_Invoke :: (SunroofArgument a, Sunroof r) => a -> JSFunction a r -> JSI t r
  JS_Eval   :: (Sunroof a) => a -> JSI t a
  JS_Function :: (SunroofArgument a, Sunroof b) => (a -> JS A b) -> JSI t (JSFunction a b)
  JS_Continuation :: (SunroofArgument a) => (a -> JS B ()) -> JSI t (JSContinuation a)
  -- Needs? Boolean bool, bool ~ BooleanOf (JS a)
  JS_Branch :: (SunroofThread t, SunroofArgument a, Sunroof bool) => bool -> JS t a -> JS t a  -> JSI t a
  JS_Return  :: (Sunroof a) => a -> JSI t ()
  JS_Assign_ :: (Sunroof a) => Id -> a -> JSI t ()
  JS_Comment :: String -> JSI t ()
  JS_Fix     :: (SunroofArgument a) => (a -> JS A a) -> JSI t a
  -- TODO: generalize Assign[_] to have a RHS

-- -------------------------------------------------------------
-- Cross the Threading Model Combinators
-- -------------------------------------------------------------

-- | Lift the atomic computation into another computation.
liftJS :: (Sunroof a) => JS A a -> JS t a
liftJS m = do
        o <- function (\ () -> m)
        apply o ()

-- -------------------------------------------------------------
-- JSFunction Type
-- -------------------------------------------------------------

-- | Type of Javascript functions.
--   The first type argument is the type of function argument.
--   This needs to be a instance of 'SunroofArgument'.
--   The second type argument of 'JSFunction' is the function return type.
--   It needs to be a instance of 'Sunroof'.
data JSFunction args ret = JSFunction Expr

instance Show (JSFunction a r) where
  show (JSFunction v) = showExpr False v

-- | Functions are first-class citizens of Javascript. Therefore they
--   are 'Sunroof' values.
instance forall a r . (SunroofArgument a, Sunroof r) => Sunroof (JSFunction a r) where
  box = JSFunction
  unbox (JSFunction e) = e
  typeOf _ = Fun (typesOf (Proxy :: Proxy a)) (typeOf (Proxy :: Proxy r))

type instance BooleanOf (JSFunction a r) = JSBool

-- | Functions may be the result of a branch.
instance (SunroofArgument a, Sunroof r) => IfB (JSFunction a r) where
  ifB = jsIfB

-- | 'JSFunction's may be created from Haskell functions if they have
--   the right form.
instance (SunroofArgument a, Sunroof b) => SunroofValue (a -> JS A b) where
  type ValueOf (a -> JS A b) = JS A (JSFunction a b)    -- TO revisit
  js = function

-- -------------------------------------------------------------
-- JSFunction Combinators
-- -------------------------------------------------------------

-- | Create a binding to a Javascript top-level function with
--   the given name. It is advised to create these bindings with an
--   associated type signature to ensure type safty while using
--   this function. Example:
--
-- > alert :: JSFunction JSString ()
-- > alert = fun "alert"
fun :: (SunroofArgument a, Sunroof r) => String -> JSFunction a r
fun = JSFunction . literal

-- | Create an 'A'tomic Javascript function from a Haskell function.
function :: (SunroofArgument a, Sunroof b) => (a -> JS A b) -> JS t (JSFunction a b)
function = single . JS_Function

infixl 1 `apply`

-- | @apply f a@ applies the function @f@ to the given arguments @a@.
--   A typical use case looks like this:
--
-- > foo `apply` (x,y)
--
--   See '$$' for a convenient infix operator to do this.
apply :: (SunroofArgument args, Sunroof ret) => JSFunction args ret -> args -> JS t ret
apply f args = f # with args
  where
    with :: (SunroofArgument a, Sunroof r) => a -> JSFunction a r -> JS t r
    with a fn = single $ JS_Invoke a fn

-- | @f $$ a@ applies the function 'f' to the given arguments @a@.
--   See 'apply'.
($$) :: (SunroofArgument args, Sunroof ret) => JSFunction args ret -> args -> JS t ret
($$) = apply

-- -------------------------------------------------------------
-- JSContinuation Type
-- -------------------------------------------------------------

-- | Type of Javascript functions.
--   The first type argument is the type of function argument.
--   This needs to be a instance of 'SunroofArgument'.
--   The second type argument of 'JSFunction' is the function return type.
--   It needs to be a instance of 'Sunroof'.
data JSContinuation args = JSContinuation Expr

instance Show (JSContinuation a) where
  show (JSContinuation v) = showExpr False v

-- | Functions are first-class citizens of Javascript. Therefore they
--   are 'Sunroof' values.
instance forall a . (SunroofArgument a) => Sunroof (JSContinuation a) where
  box = JSContinuation
  unbox (JSContinuation e) = e
  typeOf _ = Fun (typesOf (Proxy :: Proxy a)) (typeOf (Proxy :: Proxy ()))

type instance BooleanOf (JSContinuation a) = JSBool

-- | Functions may be the result of a branch.
instance (SunroofArgument a, Sunroof r) => IfB (JSContinuation a) where
  ifB = jsIfB

-- | 'JSFunction's may be created from Haskell functions if they have
--   the right form.
instance (SunroofArgument a) => SunroofValue (a -> JS B ()) where
  type ValueOf (a -> JS B ()) = JS B (JSContinuation a)    -- TO revisit
  js = continuation

-- -------------------------------------------------------------
-- JSFunction Combinators
-- -------------------------------------------------------------

-- | We can compile 'B'lockable functions that return @()@.
--   Note that, with the 'B'-style threads, we return from a
--   call when we first block, not at completion of the call.
continuation :: (SunroofArgument a) => (a -> JS B ()) -> JS t (JSContinuation a)
continuation = single . JS_Continuation

-- | @kast@ is cast to continuation. @k@ is the letter often used to signify a continuation.
kast :: (SunroofArgument a) => JSFunction a () -> JSContinuation a
kast = cast

-- Implementation of goto and callCC from
--   http://stackoverflow.com/questions/9050725/call-cc-implementation
--
-- | Reify the current contination as a Javascript continuation
callcc :: SunroofArgument a => (JSContinuation a -> JS B a) -> JS B a
callcc f = JS $ \ cc -> unJS (do o <- continuation (goto' cc)
                                 f o
                              ) cc
   where goto' :: (x ~ ()) => (a -> Program (JSI B) ()) -> a -> JS B x
         goto' cont argument = JS $ \ _ -> cont argument


-- | Abort the current computation at this point.
done :: JS t a
done = JS $ \ _ -> return ()

-- | @goto@ calls the given continuation with the given argument,
--   and never returns.
goto :: forall args a t . (SunroofArgument args) => JSContinuation args -> args -> JS t a
goto k args = JS $ \ _ -> singleton $ JS_Invoke args (cast k  :: JSFunction args ())

-- -------------------------------------------------------------
-- Basic Combinators
-- -------------------------------------------------------------

-- | Cast one Sunroof value into another.
--
--   This is sometimes needed due to Javascripts flexible type system.
cast :: (Sunroof a, Sunroof b) => a -> b
cast = box . unbox

infixr 0 #

-- | The @#@-operator is the Haskell analog to the @.@-operator
--   in Javascript. Example:
--
-- > document # getElementById "bla"
--
--   This can be seen as equivalent of @document.getElementById(\"bla\")@.
(#) :: a -> (a -> JS t b) -> JS t b
(#) obj act = act obj
-- We should use this operator for the obj.label concept.
-- It has been used in other places (but I can not seems
-- to find a library for it)

-- | Creates a selector for attributes of Javascript objects.
--   It is advised to use this together with an associated type
--   signature to avoid ambiguity. Example:
--
-- > length :: JSSelector JSNumber
-- > length = attr "length"
--
--   Selectors can be used with '!'.
attr :: String -> JSSelector a
attr a = label $ string a

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
--   Like this the flexible type signature gets fixed. See 'Language.Sunroof.Types.#'
--   for how to use these bindings.
invoke :: (SunroofArgument a, Sunroof r, Sunroof o) => String -> a -> o -> JS t r
invoke str args obj = (obj ! attr str) `apply` args

-- | @new n a@ calls the new operator on the constructor @n@
--   supplying the argument @a@. A typical use would look like this:
--
-- > new "Object" ()
--
new :: (SunroofArgument a) => String -> a -> JS t JSObject
new cons args = fun ("new " ++ cons) `apply` args

-- | Evaluate a 'Sunroof' value. This forces evaluation
--   of the given expression to a value and enables binding it to a
--   variable. Example:
--
-- > x <- evaluate $ "A" <> "B"
-- > alert x
-- > alert x
--
--   This would result in: @var v0 = \"A\"+\"B\"; alert(v0); alert(v0);@. But:
--
-- > x <- return $ "A" <> "B"
-- > alert x
-- > alert x
--
--   This will result in: @alert(\"A\"+\"B\"); alert(\"A\"+\"B\");@.
evaluate :: (Sunroof a) => a -> JS t a
evaluate a  = single (JS_Eval a)

-- | Synonym for 'evaluate'.
value :: (Sunroof a) => a -> JS t a
value = evaluate

-- | Combinator for @switch@-like statements in Javascript.
--
--   /Note/: This will not be translated into
--   actual switch statment, because you are aloud arbitrary
--   expressions in the cases.
switch :: ( EqB a, BooleanOf a ~ JSBool
          , Sunroof a, Sunroof b
          , SunroofArgument b
          , SunroofThread t
          ) => a -> [(a,JS t b)] -> JS t b
switch _a [] = return (cast (object "undefined"))
switch a ((c,t):e) = ifB (a ==* c) t (switch a e)

-- | The @null@ reference in Javascript.
nullJS :: JSObject
nullJS = box $ literal "null"

-- -------------------------------------------------------------
-- delete
-- -------------------------------------------------------------

-- | @o # delete lab@ removes the label @lab@ from the object @o@.
delete :: (Sunroof a) => JSSelector a -> JSObject -> JS t ()
delete sel o = single (JS_Delete sel o)

-- -------------------------------------------------------------
-- JSTuple Type Class
-- -------------------------------------------------------------

-- | If something is a 'JSTuple', it can easily be decomposed and
--   recomposed from different components. This is meant as a convenient
--   access to attributes of an object.
-- TODO: revisit this
class Sunroof o => JSTuple o where
        type Internals o
        match :: (Sunroof o) => o -> Internals o
        tuple :: Internals o -> JS t o

instance JSTuple JSObject where
  type Internals JSObject = ()
  match _ = ()
  tuple () = new "Object" ()


-- -------------------------------------------------------------
-- SunroofKey Type Class
-- -------------------------------------------------------------

-- | Everything that can be used as an key in a dictionary lookup.
class Sunroof key => SunroofKey key where
   jsKey :: key -> JSSelector a

-- To break the module loop
instance SunroofKey JSString where
   jsKey k = label ("$" <> cast k)

instance SunroofKey JSNumber where
   jsKey k = index k

instance SunroofKey JSBool where
   jsKey k = label (cast k)

-- -------------------------------------------------------------
-- SunroofFunctor Type Class
-- -------------------------------------------------------------

class SunroofFunctor m where
  forEach :: (Sunroof a) => (a -> JS A ()) -> m a -> JS t ()
  -- | map provided in all modern versions of JavaScript.
  jsMap :: (Sunroof a, Sunroof b) => (a -> JS A b) -> m a -> JS t (m b)


