{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances #-}

module Web.Sunroof where

import GHC.Exts
import Data.Char
import Data.Monoid
--data JSM a = JSM a      -- ...

data U where
  U :: (Sunroof a) => a -> U    -- universal

instance Show U where
  show (U a) = show a

data JSM a where
        JS_Call   :: (Sunroof a)
                  => String -> [JSValue] -> Type -> Style -> JSM a
                                                        -- direct call
        JS_Bind   :: JSM a -> (a -> JSM b) -> JSM b     -- Haskell monad bind
        JS_Return :: a -> JSM a                         -- Haskell monad return


data JSA where
    JSA :: (Sunroof a) => a -> JSA

instance Show JSA where
    show (JSA a) = show a

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
        | Op String [Expr a]

instance Show a => Show (Expr a) where
        show (Lit a)  = show a
        show (Var uq) = "v" ++ show uq
        show (Op x [a,b]) | all (not . isAlpha) x = "(" ++ show a ++ ")+(" ++ show b ++ ")"

---------------------------------------------------------------

data JSValue where
  JSValueLit :: (Sunroof a) => a -> JSValue
  JSValueVar :: Uniq -> JSValue

instance Show JSValue where
        show (JSValueLit v)  = show v
        show (JSValueVar uq) = "v" ++ show uq

instance Sunroof JSValue where
        mkVar = JSValueVar -- yes, because there is no type encoded here
        directCompile i = (show i,Value,Direct)

to :: (Sunroof a) => a -> JSValue
to = JSValueLit

-- can fail *AT RUN TIME*.
-- (check to see if we actually need it)
fromJSValue :: (Sunroof a) => JSValue -> a
fromJSValue (JSValueLit i) = error "fromJSValue"
fromJSValue (JSValueVar uq) = mkVar uq

{-
instance Show JSInt where
        show (JSValue v) = show v

instance Sunroof JSInt where
        mkVar = JSValue . Var
        directCompile i = (show i,Value,Direct)
-}
data JSInt = JSInt (Expr Int)

instance Show JSInt where
        show (JSInt v) = show v

instance Sunroof JSInt where
        mkVar = JSInt . Var
        directCompile i = (show i,Value,Direct)

instance Num JSInt where
        (JSInt e1) + (JSInt e2) = JSInt $ Op "+" [e1,e2]
        (JSInt e1) * (JSInt e2) = JSInt $ Op "*" [e1,e2]
        abs (JSInt e1) = JSInt $ Op "Math.abs" [e1]
        signum (JSInt e1) = JSInt $ Op "" [e1]
        fromInteger = JSInt . Lit . fromInteger

data JSString = JSString (Expr String)

instance Show JSString where
        show (JSString v) = show v

instance Sunroof JSString where
        mkVar = JSString . Var
        directCompile i = (show i,Value,Direct)

instance Monoid JSString where
        mempty = fromString ""
        mappend (JSString e1) (JSString e2) = JSString $ Op "+" [e1,e2]

instance IsString JSString where
    fromString = JSString . Lit

---------------------------------------------------------------
{-
data JSV a where
        JS_Var :: Int                   -> JSV a        -- named value
        JS_Int :: Int                   -> JSV Int
        JS_String :: String             -> JSV String

instance Show (JSV a) where
        show (JS_Var n) = "v" ++ show n
        show (JS_Int i) = show i
        show (JS_String s) = show s

instance Num (JSV Int) where
        fromInteger i = JS_Int (fromInteger i)

instance IsString (JSV String) where
    fromString = JS_String
-}
---------------------------------------------------------------

-- TODO: split into Show => Value => Sunroof
-- and move mkVar into the Value class.
-- This will remove the class below
class Show a => Sunroof a where
        mkVar :: Int -> a
        directCompile :: a -> (String,Type,Style)
{-

instance Sunroof (JSV Int) where
        mkVar u = JS_Var u
        directCompile i = (show i,Value,Direct)

instance Sunroof (JSV String) where
        mkVar u = JS_Var u
        directCompile i = (show i,Value,Direct)        -- TODO: fix

instance Sunroof U where
        mkVar i = U (mkVar i :: JSV Int)        -- HACK
        directCompile (U a) = directCompile a
-}
instance Sunroof () where
        mkVar _ = ()
        directCompile i = ("undefined",Unit,Direct)


-- data JS_Call = JS_Call String [JSM U] Type

data Style
        = Direct                -- just the answer
        | Continue              -- expecting the continuation to be passed, void return
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
compile (JS_Call nm args ty style) = do
        let inside = nm ++ "(" ++ commas (map show args) ++ ")"
        return (inside,ty,style)
compile (JS_Bind m1 m2) = case m1 of
        JS_Return a     -> compile (m2 a)
        JS_Bind m11 m12 -> compile (JS_Bind m11 (\ a -> JS_Bind (m12 a) m2))
        JS_Call {}      -> bind m1 m2


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
        uq <- uniqM
        let a = mkVar uq
        (txt1,ty1) <- compileC m1
        (txt2,ty2) <- compileC (m2 a)
        let lab = case ty1 of
                    Unit  -> ""  -- no name captured
                    _     -> show a
        return ("function(k){(" ++ txt1 ++ ")(function(" ++ lab ++ "){" ++ txt2 ++ "(k)})}",ty2,Continue)



commas [] = ""
commas [x] = x
commas (x:xs) = x ++ "," ++ commas xs


----------------------------------------------------------

test2 :: JSM ()
test2 = JS_Call "foo" [to (1 :: JSInt)] Value Direct

run_test2 = runCompM (compile test2) 0

test3 :: JSM ()
test3 = do
        JS_Call "foo1" [to (1 :: JSInt)] Unit Direct :: JSM ()
        (n :: JSInt) <- JS_Call "foo2" [to (2 :: JSInt)] Value Direct
        JS_Call "foo3" [to (3 :: JSInt), to n] Value Direct :: JSM ()

run_test3 = runCompM (compile test3) 0

alert :: JSString -> JSM ()
alert msg = JS_Call "alert" [to msg] Unit Direct :: JSM ()


-- This works in the browser
test4 :: JSM ()
test4 = do
        alert("A")
        alert("B")

run_test4 = runCompM (compile test4) 0

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