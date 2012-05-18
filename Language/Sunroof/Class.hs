{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances, TypeFamilies #-}

module Language.Sunroof.Class where

import GHC.Exts
import Data.Char
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Monoid

type Uniq = Int         -- used as a unique label

cast :: (Sunroof a, Sunroof b) => a -> b
cast = box . unbox

true = JSBool (Lit "true")
false = JSBool (Lit "false")

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
        showVar :: a -> String -- needed because show instance for unit is problematic
        showVar = show
        directCompile :: a -> (String,Type,Style)
        isUnit :: a -> Bool
        isUnit _ = False
        box :: Expr -> a
        unbox :: a -> Expr

--directCompile :: (Sunroof a) => (String,Type,Style)
--directCompile a = (show (unbox a),if isUnit a then

instance Sunroof () where
        mkVar _ = ()
        showVar _ = ""
        directCompile i = ("",Unit,Direct)      -- TODO: what do we do for unit?
        isUnit () = True
        box _ = ()
        unbox () = Lit ""

data JSValue where
  JSValue :: Expr -> JSValue
  JSValueVar :: Uniq -> JSValue         -- so the typing does not through a fit

instance Show JSValue where
        show (JSValue v)  = show v
--        show (JSValueVar uq) = "v" ++ show uq

instance Sunroof JSValue where
        mkVar = JSValueVar
        showVar (JSValueVar u) = error "not sure this should happen" -- "v" ++ show u
        directCompile i = (show i,Value,Direct)
        box = JSValue
        unbox (JSValue e) = e

data JSBool = JSBool Expr

instance Show JSBool where
        show (JSBool e) = show e

instance Sunroof JSBool where
        mkVar = JSBool . Var
        directCompile i = (show i,Value,Direct)
        box = JSBool
        unbox (JSBool v)  = v

data JSFunction a b = JSFunction Expr

data JSNumber = JSNumber Expr

instance Show JSNumber where
        show (JSNumber v) = show v

instance Sunroof JSNumber where
        mkVar = JSNumber . Var
        directCompile i = (show i,Value,Direct)
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

data JSString = JSString Expr

instance Show JSString where
        show (JSString v) = show v

instance Sunroof JSString where
        mkVar = JSString . Var
        directCompile i = (show i,Value,Direct)
        box = JSString
        unbox (JSString e) = e

instance Monoid JSString where
        mempty = fromString ""
        mappend (JSString e1) (JSString e2) = JSString $ Op "+" [e1,e2]

instance IsString JSString where
    fromString = JSString . Lit . show

----------------------------------------------------

data JSObject = JSObject Expr

instance Show JSObject where
        show (JSObject v@(Var {})) = show v

instance Sunroof JSObject where
        mkVar = JSObject . Var
        directCompile o = (show o,Value,Direct)
        box = JSObject
        unbox (JSObject o) = o

{-
data JSArray = JSArray Expr

instance Show JSArray where
        show (JSArray v@(Var {})) = show v

instance Sunroof JSArray where
        mkVar = JSArray . Var
        directCompile i = error "JSArray"
-}

----------------------------------------------------

data Style = Direct                -- just the answer
           | Continue              -- expecting the continuation to be passed
    deriving Show

data Type = Unit | Value deriving Show

----------------------------------------------------------
{-
-- Prelude
data JSVar a = JSVar JSObject

newJSVar :: (Sunroof a) => a -> JSM (JSVar a)
newJSVar start = do
        obj <- JS_Select $ JSS_Call "Sunroof_newJSVar" [cast start] Value Direct :: JSM JSObject
        return (JSVar obj)

readJSVar :: forall a . (Sunroof a) => JSVar a -> JSM a
readJSVar (JSVar obj) = do
        JS_Select $ JSS_Call "Sunroof_readJSVar" [cast obj] Value Direct :: JSM a

writeJSVar :: (Sunroof a) => JSVar a -> a -> JSM ()
writeJSVar (JSVar obj) val = do
        JS_Select $ JSS_Call "Sunroof_readJSVar" [cast obj,cast val] Unit Direct :: JSM ()

-- output the current commands, consider other events, etc.
flush :: JSM ()
flush = JS_Select $ JSS_Call "Sunroof_flush" [] Unit Continue :: JSM ()

alert :: JSString -> JSM ()
alert msg = JS_Select $ JSS_Call "alert" [cast msg] Unit Direct :: JSM ()

loop :: JSM () -> JSM ()
loop m = JS_Loop m

-- {-
----------------------------------------------------------
test2 :: JSM ()
test2 = JS_Select $ JSS_Call "foo" [cast (1 :: JSNumber)] Value Direct

run_test2 = runCompM (compile test2) 0

test3 :: JSM ()
test3 = do
        JS_Select $ JSS_Call "foo1" [cast (1 :: JSNumber)] Unit Direct :: JSM ()
        (n :: JSNumber) <- JS_Select $ JSS_Call "foo2" [cast (2 :: JSNumber)] Value Direct
        JS_Select $ JSS_Call "foo3" [cast (3 :: JSNumber), cast n] Value Direct :: JSM ()

run_test3 = runCompM (compile test3) 0


-- This works in the browser
test4 :: JSM ()
test4 = do
        alert("A")
        alert("B")

run_test4 = runCompM (compile test4) 0

foo :: JSNumber -> JSS ()
foo msg = JSS_Call "foo" [cast msg] Value Direct :: JSS ()

test5 :: JSM ()
test5 = do
        let c = mkVar 0 :: JSObject
        c <$> foo (1)
        return ()

run_test5 = runCompM (compile test5) 0
-- -}
-}
