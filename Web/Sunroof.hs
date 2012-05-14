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
        JS_Assign :: JSObject -> String -> JSValue -> JSM () -- obj . x = <exp>;

        JS_Dot    :: (Sunroof a)
                  => JSObject -> JSS a -> JSM a            -- obj . <selector>

infixl 4 <*>
infixl 4 <$>

(<*>) :: (Sunroof a) => JSM JSObject -> JSS a -> JSM a
(<*>) m s = m >>= \ o -> o <$> s

(<$>) :: (Sunroof a) => JSObject -> JSS a -> JSM a
(<$>) o s = o `JS_Dot` s

(!) :: forall a . (Sunroof a) => JSArray -> JSInt -> a
(!) arr idx = from $ JSValue (Op "[]" [to arr,to idx] :: Expr a)

data JSS a where
        JSS_Call   :: String -> [JSValue] -> Type -> Style -> JSS a
        JSS_Select :: String ->              Type ->          JSS a

instance Monad JSM where
        return = JS_Return
        (>>=) = JS_Bind

---------------------------------------------------------------

type Uniq = Int         -- used as a unique label

---------------------------------------------------------------
-- Trivial expression language
data Expr a
        = Lit a
        | Var Uniq
        | Op String [JSValue]
        | Cast JSValue

instance Show a => Show (Expr a) where
        show (Lit a)  = show a
        show (Var uq) = "v" ++ show uq
        show (Op "[]" [a,x]) = "(" ++ show a ++ ")[" ++ show x ++ "]"
        show (Op x [a,b]) | all (not . isAlpha) x = "(" ++ show a ++ ")+(" ++ show b ++ ")"
        show (Cast e) = show e

---------------------------------------------------------------

data Void

data JSValue where
  JSValue :: (Sunroof a) => Expr a -> JSValue
  JSValueVar :: Uniq -> JSValue         -- so the typing does not through a fit

instance Show JSValue where
        show (JSValue v)  = show v
--        show (JSValueVar uq) = "v" ++ show uq

instance Sunroof JSValue where
        mkVar = JSValueVar
        directCompile i = (show i,Value,Direct)

to :: (Sunroof a) => a -> JSValue
to = JSValue . Lit

a = 99 :: JSInt
b = to a :: JSValue
c = from b :: JSInt

-- can fail *AT RUN TIME*.
from :: (Sunroof a) => JSValue -> a
from v = box (Cast v)

data JSInt = JSInt (Expr Int)

instance Show JSInt where
        show (JSInt v) = show v

instance Sunroof JSInt where
        mkVar = JSInt . Var
        directCompile i = (show i,Value,Direct)
        type Internal JSInt = Int
        box = JSInt

instance Num JSInt where
        e1 + e2 = JSInt $ Op "+" [to e1,to e2]
        e1 - e2 = JSInt $ Op "-" [to e1,to e2]
        e1 * e2 = JSInt $ Op "*" [to e1,to e2]
        abs e1 = JSInt $ Op "Math.abs" [to e1]
        signum e1 = JSInt $ Op "" [to e1]
        fromInteger = JSInt . Lit . fromInteger

data JSString = JSString (Expr String)

instance Show JSString where
        show (JSString v) = show v

instance Sunroof JSString where
        mkVar = JSString . Var
        directCompile i = (show i,Value,Direct)

instance Monoid JSString where
        mempty = fromString ""
        mappend e1 e2 = JSString $ Op "+" [to e1,to e2]

instance IsString JSString where
    fromString = JSString . Lit

----------------------------------------------------

data JSObject = JSObject (Expr (Map.Map String JSValue))

instance Show JSObject where
        show (JSObject v@(Var {})) = show v

instance Sunroof JSObject where
        mkVar = JSObject . Var
        directCompile o = (show o,Value,Direct)

data JSArray = JSArray (Expr (Map.Map Int JSValue))

instance Show JSArray where
        show (JSArray v@(Var {})) = show v

instance Sunroof JSArray where
        mkVar = JSArray . Var
        directCompile i = error "JSArray"

----------------------------------------------------

class Show a => Sunroof a where
        mkVar :: Int -> a
        directCompile :: a -> (String,Type,Style)
        type Internal a
        box :: Expr (Internal a) -> a

instance Sunroof () where
        mkVar _ = ()
        directCompile i = ("",Unit,Direct)      -- TODO: what do we do for unit?
        type Internal () = ()
        box _ = ()

data Style
        = Direct                -- just the answer
        | Continue              -- expecting the continuation to be passed
        deriving Show

data Type
        = Unit
        | Value
        | Object
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
        JS_Assign {}    -> bind m1 m2

compileJSS :: (Sunroof a) => JSS a -> CompM (String,Type,Style)
compileJSS (JSS_Call nm args ty style) = do
        let inside = nm ++ "(" ++ commas (map show args) ++ ")"
        return (inside,ty,style)
compileJSS (JSS_Select nm ty) = do
        return (nm,ty,Direct)


-- a version of compile that always returns CPS form.
compileC :: (Sunroof a) => JSM a -> CompM (String,Type)
compileC a = do
        (txt,ty,style) <- compile a
        case style of
           Direct -> return ("(function(k){k(" ++ txt ++ ")})",ty)
           Continue -> return (txt,ty)

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

----------------------------------------------------------

test2 :: JSM ()
test2 = JS_Select $ JSS_Call "foo" [to (1 :: JSInt)] Value Direct

run_test2 = runCompM (compile test2) 0

test3 :: JSM ()
test3 = do
        JS_Select $ JSS_Call "foo1" [to (1 :: JSInt)] Unit Direct :: JSM ()
        (n :: JSInt) <- JS_Select $ JSS_Call "foo2" [to (2 :: JSInt)] Value Direct
        JS_Select $ JSS_Call "foo3" [to (3 :: JSInt), to n] Value Direct :: JSM ()

run_test3 = runCompM (compile test3) 0

alert :: JSString -> JSM ()
alert msg = JS_Select $ JSS_Call "alert" [to msg] Unit Direct :: JSM ()


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