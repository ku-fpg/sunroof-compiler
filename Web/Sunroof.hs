{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes, FlexibleInstances #-}

module Web.Sunroof where

--data JSM a = JSM a      -- ...

data U where
  U :: (Sunroof a) => a -> U    -- universal

instance Show U where
  show (U a) = show a

data JSM a where
        JS_Action :: (Sunroof a) => JS_Call -> JSM a       -- direct call; no returned value.

        JS_Bind   :: JSM a -> (a -> JSM b) -> JSM b     -- Haskell monad bind
        JS_Return :: a -> JSM a                         -- Haskell monad return

instance Monad JSM where
        return = JS_Return
        (>>=) = JS_Bind

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


---------------------------------------------------------------



class Show a => Sunroof a where
        mkVar :: Int -> a
        directCompile :: a -> (String,Type,Style)

instance Sunroof (JSV Int) where
        mkVar u = JS_Var u
        directCompile i = (show i,Number,Direct)

instance Sunroof U where
--        mkVar i = U (mkVar i)
        directCompile (U a) = directCompile a

instance Sunroof () where
        mkVar _ = ()
        directCompile i = ("{}",Unit,Direct)




data JS_Call = JS_Call String [JSM U] Type

data Style
        = Direct                -- just the answer
        | Continue              -- expecting the continuation to be passed, void return
        deriving Show

data Type
        = Unit
        | Number
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
compile (JS_Action call) = compileCall call
compile (JS_Bind m1 m2) =
    case m1 of
        JS_Return a     -> compile (m2 a)
        JS_Bind m11 m12 -> compile (JS_Bind m11 (\ a -> JS_Bind (m12 a) m2))
        JS_Action call  -> bind (JS_Action call) m2


-- a version of compile that always returns CPS

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
                    Unit -> ""  -- no name captured
                    _ -> show a
        return ("function(k){(" ++ txt1 ++ ")(function(" ++ lab ++ "){" ++ txt2 ++ "(k)})}",ty2,Continue)

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


commas [] = ""
commas [x] = x
commas (x:xs) = x ++ "," ++ commas xs

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

test2 :: JSM ()
test2 = JS_Action (JS_Call "foo" [return (U (1 :: JSV Int))] Number)

run_test2 = runCompM (compile test2) 0

test3 :: JSM ()
test3 = do
        JS_Action (JS_Call "foo1" [return (U (1 :: JSV Int))] Unit) :: JSM ()
        (n :: JSV Int) <- JS_Action (JS_Call "foo2" [return (U (2 :: JSV Int))] Number)
        JS_Action (JS_Call "foo3" [return (U (3 :: JSV Int)), return (U n)] Number) :: JSM ()

run_test3 = runCompM (compile test3) 0

{-
-- :: C[[JSM a]] => k -> ()
bind :: JSM a -> (a -> JSM b) -> JSM b
bind (
-}


