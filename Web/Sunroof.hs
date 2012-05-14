{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances, TypeFamilies #-}

module Web.Sunroof where

import GHC.Exts
import Data.Char
import qualified Data.Map as Map
import Data.Monoid
--data JSM a = JSM a      -- ...

data U where
  U :: (Sunroof a) => a -> U    -- universal

instance Show U where
  show (U a) = show a

data JSM a where
        JS_Bind   :: JSM a -> (a -> JSM b) -> JSM b     -- Haskell monad bind
        JS_Return :: a -> JSM a                         -- Haskell monad return

        JS_Select :: (Sunroof a)
                  => JSS a -> JSM a
--                  String -> [JSValue] -> Type -> Style
                                                           -- direct call
        JS_Dot    :: (Sunroof a)
                  => JSObject -> JSS a -> JSM a            -- obj . <selector>

        -- You can build functions, and pass them round
        JS_Function :: (Sunroof a, Sunroof b)
                    => (a -> JSM b)
                    -> JSM (JSFunction a b)
        -- You can invoke functions
        JS_Invoke :: JSFunction a b -> a -> JSM b

infixl 4 <*>
infixl 4 <$>
infix  5 :=

(<*>) :: (Sunroof a) => JSM JSObject -> JSS a -> JSM a
(<*>) m s = m >>= \ o -> o <$> s

(<$>) :: (Sunroof a) => JSObject -> JSS a -> JSM a
(<$>) o s = o `JS_Dot` s

--(!) :: forall a . (Sunroof a) => JSArray -> JSInt -> a
--(!) arr idx = from $ JSValue (Op "[]" [to arr,to idx] :: Expr a)

data JSS a where
        JSS_Call   :: String -> [JSValue] -> Type -> Style -> JSS a
        JSS_Select :: String ->              Type ->          JSS a
        (:=)       :: (Sunroof a) => JSF a -> a ->            JSS ()

data JSF a where
        JSF_Field  :: String -> JSF a

field :: String -> JSF a
field = JSF_Field

instance Monad JSM where
        return = JS_Return
        (>>=) = JS_Bind

while :: JSM JSBool -> JSM ()
while body = return ()

cond :: JSM JSBool -> JSM () -> JSM ()
cond _ _ = return ()

---------------------------------------------------------------

type Uniq = Int         -- used as a unique label

---------------------------------------------------------------
-- Trivial expression language
data Expr
        = Lit String    -- a precompiled version of this literal
        | Var Uniq
        | Op String [Expr]
        | Cast JSValue

instance Show Expr where
        show (Lit a)  = a
        show (Var uq) = "v" ++ show uq
        show (Op "[]" [a,x]) = "(" ++ show a ++ ")[" ++ show x ++ "]"
        show (Op x [a,b]) | all (not . isAlpha) x = "(" ++ show a ++ ")+(" ++ show b ++ ")"
        show (Cast e) = show e

---------------------------------------------------------------

data Void

data JSValue where
  JSValue :: Expr -> JSValue
  JSValueVar :: Uniq -> JSValue         -- so the typing does not through a fit

instance Show JSValue where
        show (JSValue v)  = show v
--        show (JSValueVar uq) = "v" ++ show uq

instance Sunroof JSValue where
        mkVar = JSValueVar
        directCompile i = (show i,Value,Direct)
        box = JSValue
        unbox (JSValue e) = e

cast :: (Sunroof a, Sunroof b) => a -> b
cast = box . unbox

-- todo, remove
to :: (Sunroof a) => a -> JSValue
to = cast

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

class Show a => Sunroof a where
        mkVar :: Int -> a
        directCompile :: a -> (String,Type,Style)
        isUnit :: a -> Bool
        isUnit _ = False
        box :: Expr -> a
        unbox :: a -> Expr

--directCompile :: (Sunroof a) => (String,Type,Style)
--directCompile a = (show (unbox a),if isUnit a then

instance Sunroof () where
        mkVar _ = ()
        directCompile i = ("",Unit,Direct)      -- TODO: what do we do for unit?
        isUnit () = True
        box _ = ()
        unbox () = Lit ""

data Style
        = Direct                -- just the answer
        | Continue              -- expecting the continuation to be passed
        deriving Show

data Type
        = Unit
        | Value
--        | Object
        deriving Show

newtype CompM a = CompM { runCompM :: Int -> (a,Int) }

instance Monad CompM where
        return a = CompM $ \ u -> (a,u)
        (CompM m) >>= k = CompM $ \ u0->
                        let (a,u1) = m u0
                        in runCompM (k a) u1

uniqM :: CompM Int
uniqM = CompM $ \ u -> (u,succ u)

compile :: (Sunroof a) => JSM a -> CompM (String,Type,Style)
compile (JS_Return a) = return $ directCompile a
compile (JS_Select jss) = compileJSS jss
compile (JS_Dot o jss) = do
        (sel_txt,ty,style) <- compileJSS jss
        let (o_txt,_,_) = directCompile o
        return ("(" ++ o_txt ++ ")." ++ sel_txt,ty,style)

compile (JS_Bind m1 m2) = case m1 of
        JS_Return a     -> compile (m2 a)
        JS_Bind m11 m12 -> compile (JS_Bind m11 (\ a -> JS_Bind (m12 a) m2))
        JS_Select {}    -> bind m1 m2
        JS_Dot    {}    -> bind m1 m2
--        JS_Assign {}    -> bind m1 m2

compileJSS :: (Sunroof a) => JSS a -> CompM (String,Type,Style)
compileJSS (JSS_Call nm args ty style) = do
        -- This show is doing the compile
        let inside = nm ++ "(" ++ commas (map show args) ++ ")"
        return (inside,ty,style)
compileJSS (JSS_Select nm ty) = do
        return (nm,ty,Direct)
compileJSS ((JSF_Field nm) := arg) = do
        return (nm ++ " = (" ++ show arg ++ ")",Unit,Direct)


-- a version of compile that always returns CPS form.
compileC :: (Sunroof a) => JSM a -> CompM (String,Type)
compileC a = do
        (txt,ty,style) <- compile a
        case (style,ty) of
           (Direct,Unit)  -> return ("(function(k){" ++ txt ++ ";k();})",ty)
           (Direct,Value) -> return ("(function(k){k(" ++ txt ++ ")})",ty)
           _ -> return (txt,ty)

-- This is the magic bit, where the argument passed to the second argument
-- is constructed out of think air.

bind :: (Sunroof a, Sunroof b) => JSM b -> (b -> JSM a) -> CompM (String,Type,Style)
bind m1 m2 = do
        (txt1,ty1) <- compileC m1
        a <- newVar
        (txt2,ty2) <- compileC (m2 a)
        case ty1 of
          Unit -> return ("function(k){(" ++ txt1 ++ ")(function(){(" ++ txt2 ++ ")(k)})}",ty2,Continue)
          _    -> return ("function(k){(" ++ txt1 ++ ")(function(" ++ show a ++ "){(" ++ txt2 ++ ")(k)})}",ty2,Continue)

newVar :: (Sunroof a) => CompM a
newVar = do
        uq <- uniqM
        return $ mkVar uq


commas [] = ""
commas [x] = x
commas (x:xs) = x ++ "," ++ commas xs

----------------------------------------------------------
-- Prelude
data JSVar a = JSVar JSObject

newJSVar :: (Sunroof a) => a -> JSM (JSVar a)
newJSVar start = do
        obj <- JS_Select $ JSS_Call "Sunroof_newJSVar" [to start] Value Direct :: JSM JSObject
        return (JSVar obj)

readJSVar :: forall a . (Sunroof a) => JSVar a -> JSM a
readJSVar (JSVar obj) = do
        JS_Select $ JSS_Call "Sunroof_readJSVar" [to obj] Value Direct :: JSM a

writeJSVar :: (Sunroof a) => JSVar a -> a -> JSM ()
writeJSVar (JSVar obj) val = do
        JS_Select $ JSS_Call "Sunroof_readJSVar" [to obj,to val] Unit Direct :: JSM ()

-- output the current commands, consider other events, etc.
flush :: JSM ()
flush = JS_Select $ JSS_Call "Sunroof_flush" [] Unit Continue :: JSM ()

alert :: JSString -> JSM ()
alert msg = JS_Select $ JSS_Call "alert" [to msg] Unit Direct :: JSM ()

----------------------------------------------------------
{-
test2 :: JSM ()
test2 = JS_Select $ JSS_Call "foo" [to (1 :: JSInt)] Value Direct

run_test2 = runCompM (compile test2) 0

test3 :: JSM ()
test3 = do
        JS_Select $ JSS_Call "foo1" [to (1 :: JSInt)] Unit Direct :: JSM ()
        (n :: JSInt) <- JS_Select $ JSS_Call "foo2" [to (2 :: JSInt)] Value Direct
        JS_Select $ JSS_Call "foo3" [to (3 :: JSInt), to n] Value Direct :: JSM ()

run_test3 = runCompM (compile test3) 0


-- This works in the browser
test4 :: JSM ()
test4 = do
        alert("A")
        alert("B")

run_test4 = runCompM (compile test4) 0

foo :: JSInt -> JSS ()
foo msg = JSS_Call "foo" [to msg] Value Direct :: JSS ()

test5 :: JSM ()
test5 = do
        let c = mkVar 0 :: JSObject
        c <$> foo (1)
        return ()

run_test5 = runCompM (compile test5) 0
-}
----------------------------------------------------------
-- Old stuff
compileArgs :: [(String,Type,Style)] -> CompM (String,[String],String)
compileArgs [] = return ("",[],"")
compileArgs ((arg_txt,arg_ty,style):rest) = do
        (pre,args,post) <- compileArgs rest
        let n = length rest
            v = "v" ++ show n
        case style of
           Direct -> return(pre,arg_txt : args,post)
           Continue -> return("(" ++ arg_txt ++ ")(function(" ++ v ++ "){" ++ pre,v : args,post ++ "})")
{-
        case arg_style of
          Direct   -> return (pre,arg_txt : args,style)
          Continue -> return (...,...,Continue)
--          Continue ->
-}

{-
directToContinue :: String -> String
directToContinue dir = ""

continueToDirect :: String -> String
continueToDirect cont = "(function(){" ++ contnm ++ "(" ++ commas args ++ ");" ++ post ++ "})()"
-}
{-
compile (JS_Bind (JS_Bind m1 m2) m3) =

compile (JS_Bind m k) = do
        (txt,ty,style) <- compile (hack m)
        return $ error "X"

hack :: JSM a -> JSM U
hack (JS_Return {}) = error "can not be return"
hack (JS_Bind {}) = error "can not be return"
hack (JS_Action call) = JS_Action call

-}

{-
compileCall (JS_Call nm args ty) = do
        res <- mapM compile args
        -- if they are all direct, we can
        -- Assumption for now
        (pre,args,post) <- compileArgs res
        let inside = nm ++ "(" ++ commas args ++ ")"
        if null pre && null post
                then return (inside,ty,Direct)
                        -- TODO: add return if the value is not Unit
                        -- I think this will make it a Continue
                else return ("(function(){" ++ pre ++ inside ++ ";" ++ post ++ "})()",ty,Direct)

-}