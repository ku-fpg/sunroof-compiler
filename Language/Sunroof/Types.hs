{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances, TypeFamilies #-}

module Language.Sunroof.Types where

import GHC.Exts
import Data.Char
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.Operational
import Web.KansasComet (Template(..), extract)
import Data.Boolean
import Web.KansasComet

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
--        show (Cast e) = show e

---------------------------------------------------------------

class Show a => Sunroof a where
        mkVar :: Uniq -> a
        box :: Expr -> a
        unbox :: a -> Expr

        showVar :: a -> String -- needed because show instance for unit is problematic
        showVar = show

        assignVar :: a -> String
        assignVar a = "var " ++ show a ++ "="

-- unit is the oddball
instance Sunroof () where
        mkVar _ = ()
        showVar _ = ""
        assignVar _ = ""
        box _ = ()
        unbox () = Lit ""

{-
instance (Sunroof a, Sunroof b) => Sunroof (a,b) where
    mkVar u = let (x,u') = mkVar u
                  (y,u'') = mkVar u'
              in ((x,y), u'')
    box (Op o [x,y]) | o == "(,)" = (box x, box y)
    unbox (x,y) = Op "(,)" [unbox x, unbox y]
    showVar (x,y) = showVar x ++ "," ++ showVar y -- tuples get flattened into sep arguments

    -- javascript's crazy scoping actually comes in handy
    assignVar (x,y) = "var " ++ show x ++ "; var " ++ show y ++ "; function(a1,a2){" ++ show x ++ "=a1;" ++ show y ++ "=a2})"
-}

---------------------------------------------------------------

data JSValue where
  JSValue :: Expr -> JSValue
  JSValueVar :: Uniq -> JSValue         -- so the typing does not throw a fit

instance Show JSValue where
        show (JSValue v)  = show v
--        show (JSValueVar uq) = "v" ++ show uq

instance Sunroof JSValue where
        mkVar = JSValueVar
        showVar (JSValueVar u) = error "not sure this should happen" -- "v" ++ show u
        box = JSValue
        unbox (JSValue e) = e

instance IsString JSValue where
    fromString = JSValue . Lit . show

---------------------------------------------------------------

data JSBool = JSBool Expr

instance Show JSBool where
        show (JSBool e) = show e

instance Sunroof JSBool where
        mkVar = JSBool . Var
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


instance IfB JSBool where
    type BooleanOf JSBool = JSBool
    ifB = js_ifB

js_ifB (JSBool c) t e = box $ Op "?:" [c,unbox t,unbox e]

---------------------------------------------------------------

-- The type argument of JSFunction is what the function returns.
data JSFunction ret = JSFunction Expr

instance Show (JSFunction a) where
        show (JSFunction v) = show v

instance Sunroof (JSFunction a) where
        mkVar = JSFunction . Var
        box = JSFunction
        unbox (JSFunction e) = e


instance IfB (JSFunction a) where
    type BooleanOf (JSFunction a) = JSBool
    ifB = js_ifB

---------------------------------------------------------------

data JSNumber = JSNumber Expr

instance Show JSNumber where
        show (JSNumber v) = show v

instance Sunroof JSNumber where
        mkVar = JSNumber . Var
        box = JSNumber
        unbox (JSNumber e) = e

instance Num JSNumber where
        (JSNumber e1) + (JSNumber e2) = JSNumber $ Op "+" [e1,e2]
        (JSNumber e1) - (JSNumber e2) = JSNumber $ Op "-" [e1,e2]
        (JSNumber e1) * (JSNumber e2) = JSNumber $ Op "*" [e1,e2]
        abs (JSNumber e1) = JSNumber $ Op "Math.abs" [e1]
        signum (JSNumber e1) = JSNumber $ Op "" [e1]
        fromInteger = JSNumber . Lit . show . fromInteger

instance Fractional JSNumber where
        (JSNumber e1) / (JSNumber e2) = JSNumber $ Op "/" [e1,e2]
        fromRational = JSNumber . Lit . show . fromRational

instance Floating JSNumber where
        pi = JSNumber $ Lit $ "Math.PI"

instance IfB JSNumber where
    type BooleanOf JSNumber = JSBool
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
        mkVar = JSString . Var
        box = JSString
        unbox (JSString e) = e

instance Monoid JSString where
        mempty = fromString ""
        mappend (JSString e1) (JSString e2) = JSString $ Op "+" [e1,e2]

instance IsString JSString where
    fromString = JSString . Lit . show

instance IfB JSString where
    type BooleanOf JSString = JSBool
    ifB = js_ifB

instance EqB JSString where
    (==*) e1 e2 = JSBool $ Op "==" [unbox e1,unbox e2]
    (/=*) e1 e2 = JSBool $ Op "!=" [unbox e1,unbox e2]

---------------------------------------------------------------

data JSObject = JSObject Expr

instance Show JSObject where
        show (JSObject v) = show v

instance Sunroof JSObject where
        mkVar = JSObject . Var
        box = JSObject
        unbox (JSObject o) = o

instance IfB JSObject where
    type BooleanOf JSObject = JSBool
    ifB = js_ifB

---------------------------------------------------------------

-- | a 'JSSelector' selects a field from a JSObject.
-- The phantom is the type of the selected value.
data JSSelector :: * -> * where
        JSSelector :: JSString           -> JSSelector a

instance IsString (JSSelector a) where
    fromString = JSSelector . fromString

label :: JSString -> JSSelector a
label = JSSelector

---------------------------------------------------------------

(!) :: forall a . (Sunroof a) => JSObject -> JSSelector a -> a
(!) arr (JSSelector idx) = cast $ JSValue $ Op "[]" [unbox arr,unbox idx]

---------------------------------------------------------------

infix  5 :=

data Action :: * -> * -> * where
   -- Invoke is not quite right
   Invoke :: [JSValue]                                          -> Action (JSFunction a) a
   -- Basically, this is special form of call, to assign to a field
   (:=)   :: (Sunroof a) => String -> a                         -> Action JSObject ()
   -- This is the fmap-like function, an effect-free modifier on the first argument
   Map :: (Sunroof b) => (a -> b) -> Action b c                 -> Action a c

---------------------------------------------------------------

method :: JSSelector (JSFunction a) -> [JSValue] -> Action JSObject a
method str args = (! str) `Map` with args

string :: String -> JSString
string = JSString . Lit . show

object :: String -> JSObject
object = JSObject . Lit

-- perhaps call this invoke
call :: String -> JSFunction a
call = JSFunction . Lit

with :: [JSValue] -> Action (JSFunction a) a
with = Invoke

new :: JS JSObject
new = eval $ object "new Object()"

--vector :: [JSValue] -> JSVector
--vector = ...

---------------------------------------------------------------

eval :: (Sunroof a) => a -> JS a
eval a  = singleton (JS_Eval a)

loop :: a -> (a -> JS a) -> JS ()
loop a f = singleton (JS_Loop a f)

---------------------------------------------------------------

-- Control.Monad.Operational makes a monad out of JS for us
type JS a = Program JSI a

-- define primitive effects / "instructions" for the JS monad
data JSI a where

    -- apply an action to an 'a', and compute a b
    JS_App    :: (Sunroof a, Sunroof b) => a -> Action a b      -> JSI b

    -- Not the same as return; does evaluation of argument
    JS_Eval   :: (Sunroof a) => a                               -> JSI a

    -- special primitives
--    JS_Wait   :: Template a -> (JSObject -> JS ())              -> JSI ()
    JS_Loop :: a -> (a -> JS a)                                 -> JSI ()
    JS_Function :: (Sunroof a, Sunroof b) => (a -> JS b)        -> JSI (JSFunction b)

---------------------------------------------------------------

-- We only can compile functions that do not have interesting return
-- values, so we can assume they are continuation-like things.
function :: (Sunroof a, Sunroof b) => (a -> JS b) -> JS (JSFunction b)
function = singleton . JS_Function

infixl 4 <$>

(<$>) :: (Sunroof a, Sunroof b) => a -> Action a b -> JS b
(<$>) o s = singleton $ o `JS_App` s

(<*>) :: (Sunroof a, Sunroof b) => JS a -> Action a b -> JS b
(<*>) m s = m >>= \ o -> singleton $ o `JS_App` s


instance forall a . (Sunroof a) => IfB (Program JSI a) where
    type BooleanOf (Program JSI a) = JSBool
    -- I expect this should be a JS primitive, but we *can* do it without the prim
    ifB i h e = do
      h_f <- function $ \ () -> h
      e_f <- function $ \ () -> e
      o <- new
      o <$> "true" := h_f
      o <$> "false" := e_f
      o ! (label (cast i) :: JSSelector (JSFunction a)) <$> with []
--      return ()

switch :: (EqB a, BooleanOf a ~ JSBool, Sunroof a, Sunroof b) => a -> [(a,JS b)] -> JS b
switch a [] = return (cast (object "undefined"))
switch a ((c,t):e) = ifB (a ==* c) t (switch a e)


---------------------------------------------------------------

