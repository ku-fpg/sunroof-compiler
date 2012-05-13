{-# LANGUAGE OverloadedStrings, GADTs, ScopedTypeVariables, RankNTypes #-}

module Web.Sunroof where

--data JSM a = JSM a      -- ...

data U where
  U :: (Sunroof a) => a -> U    -- universal

--data JSM a where
--        JS_Double :: Double -> JSM Double
--        JS_Int    :: Int    -> JSM Int

data JSM a where
        JS_Int    :: Int    -> JSM Int
--        JS_Call   :: String  -> [JSM U] -> JSM a        -- direct return
--        JS_Cont   :: String  -> [JSM U] -> JSM a        -- returns a continuation
        JS_Action :: JS_Call -> JSM ()       -- direct call; no returned value.

        JS_Bind   :: JSM a -> (a -> JSM b) -> JSM b     -- Haskell monad bind
        JS_Return :: a -> JSM a                         -- Haskell monad return
--        JS_Val    :: String             -> JSM a        -- named value


class Value a where
        mkVar :: Int -> a

instance Value () where
        mkVar _ = ()

instance Monad JSM where
        return = JS_Return
        (>>=) = JS_Bind


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

newtype CompM a = CompM { runCompM :: a }
        deriving Show
instance Monad CompM where
        return a = CompM a
        (CompM m) >>= k = k m

class Sunroof a where
        directCompile :: a -> (String,Type,Style)

compile :: (Sunroof a) => JSM a -> CompM (String,Type,Style)
compile (JS_Int i)    = return (show i,Number,Direct)
compile (JS_Return a) = return $ directCompile a
compile (JS_Action call) = compileCall call
compile (JS_Bind m1 m2) =
    case m1 of
        JS_Return a     -> compile (m2 a)
        JS_Bind m11 m12 -> compile (JS_Bind m11 (\ a -> JS_Bind (m12 a) m2))
        JS_Action call  -> bind (JS_Action call) m2

bind :: (Sunroof a, Sunroof b, Value b) => JSM b -> (b -> JSM a) -> CompM (String,Type,Style)
bind m1 m2 = do
        -- you know that the argument is ()
        (txt1,ty1,style1) <- compile m1
        (txt2,ty2,style2) <- compile (m2 (mkVar 0))
        case (style1,style2) of
          (Direct,Direct) ->
                  return ("(function(){" ++ txt1 ++ ";" ++ txt2 ++ ";})()",ty2,Direct)


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
        (pre,args,style) <- compileArgs res

        return (pre ++ nm ++ "(" ++ commas args ++ ")",ty,style)

commas [] = ""
commas [x] = x
commas (x:xs) = x ++ "," ++ commas xs

compileArgs :: [(String,Type,Style)] -> CompM (String,[String],Style)
compileArgs [] = return ("",[],Direct)
compileArgs ((arg_txt,arg_ty,arg_style):rest) = do
        (pre,args,style) <- compileArgs rest
        case arg_style of
          Direct   -> return (pre,arg_txt : args,style)
--          Continue ->

test1 :: JSM Int
test1 = JS_Int 1

run_test1 = runCompM (compile test1)

test2 :: JSM ()
test2 = JS_Action (JS_Call "foo" [return (U (1 :: Int))] Number)

run_test2 = runCompM (compile test2)

test3 :: JSM ()
test3 = do
        JS_Action (JS_Call "foo1" [return (U (1 :: Int))] Number)
        JS_Action (JS_Call "foo2" [return (U (2 :: Int))] Number)
        JS_Action (JS_Call "foo3" [return (U (3 :: Int)), return (U (4::Int))] Number)

run_test3 = runCompM (compile test3)

instance Sunroof Int where
        directCompile i = (show i,Number,Direct)

instance Sunroof U where
        directCompile (U a) = directCompile a

instance Sunroof () where
        directCompile i = ("{}",Unit,Direct)

{-
-- :: C[[JSM a]] => k -> ()
bind :: JSM a -> (a -> JSM b) -> JSM b
bind (
-}


