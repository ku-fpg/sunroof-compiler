{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances, TypeFamilies #-}

module Language.Sunroof.Types where

import GHC.Exts
import Data.Char
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.Operational
import Web.KansasComet (Template(..), extract)


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

---------------------------------------------------------------

data JSBool = JSBool Expr

instance Show JSBool where
        show (JSBool e) = show e

instance Sunroof JSBool where
        mkVar = JSBool . Var
        box = JSBool
        unbox (JSBool v)  = v

---------------------------------------------------------------

data JSFunction ret = JSFunction Expr

--data JSFunction :: * -> * where
--        JSFunction ::                   JSFunction a

instance Show (JSFunction a) where
        show (JSFunction v) = show v

instance Sunroof (JSFunction a) where
        mkVar = JSFunction . Var
        box = JSFunction
        unbox (JSFunction e) = e

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

---------------------------------------------------------------

data JSObject = JSObject Expr

instance Show JSObject where
        show (JSObject v) = show v

instance Sunroof JSObject where
        mkVar = JSObject . Var
        box = JSObject
        unbox (JSObject o) = o

---------------------------------------------------------------

instance IsString (JSSelector a) where
    fromString = JSSelector . fromString

data JSSelector :: * -> * where
        JSSelector :: JSString           -> JSSelector a


{-
data JSArray = JSArray Expr

instance Show JSArray where
        show (JSArray v@(Var {})) = show v

instance Sunroof JSArray where
        mkVar = JSArray . Var
-}

---------------------------------------------------------------

(!) :: forall a . (Sunroof a) => JSObject -> JSSelector a -> a
(!) arr (JSSelector idx) = cast $ JSValue $ Op "[]" [unbox arr,unbox idx]

---------------------------------------------------------------

infix  5 :=

data Action :: * -> * -> * where
   Invoke :: [JSValue]                                          -> Action (JSFunction a) a
   -- Basically, this is special form of call, to assign to a field
   (:=)   :: (Sunroof a) => String -> a                         -> Action JSObject ()
   -- This is the fmap-like function, an effect-free modifier on the first argument
   Map :: (Sunroof b) => (a -> b) -> Action b c                 -> Action a c

---------------------------------------------------------------

method :: JSSelector (JSFunction a) -> [JSValue] -> Action JSObject a
method str args = (! str) `Map` with args

object :: String -> JSObject
object = JSObject . Lit

function :: String -> JSFunction a
function = JSFunction . Lit

with :: [JSValue] -> Action (JSFunction a) a
with = Invoke

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
    JS_Wait   :: Template a                                     -> JSI JSObject
    JS_Loop :: JS ()                                            -> JSI ()


---------------------------------------------------------------

--(<$>) :: (Sunroof a, Sunroof b) => a -> Action a b -> JS b
--(<$>) o s = singleton $ o `JS_App` s


---------------------------------------------------------------

