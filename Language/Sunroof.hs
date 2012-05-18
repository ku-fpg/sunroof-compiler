{-# LANGUAGE GADTs, RankNTypes #-}
module Language.Sunroof where

import qualified Control.Applicative as App
import Control.Monad.Operational
import Control.Monad.State
import Data.List (intercalate)

import Language.Sunroof.Class

infixl 4 <*>
infixl 4 <$>
infix  5 :=

(<*>) :: (Sunroof a) => JSM JSObject -> JSS a -> JSM a
(<*>) m s = m >>= \ o -> o <$> s

(<$>) :: (Sunroof a) => JSObject -> JSS a -> JSM a
(<$>) o s = singleton $ o `JS_Dot` s

(!) :: forall a . (Sunroof a) => JSObject -> JSString -> a
(!) arr idx = cast $ JSValue $ Op "[]" [unbox arr,unbox idx]

field :: String -> JSF a
field = JSF_Field

loop :: JSM () -> JSM ()
loop = singleton . JS_Loop

-- define primitive effects / "instructions" for the JSM monad
data JSMI a where
    -- selector (function, property, or property assignment)
    JS_Select :: (Sunroof a) => JSS a -> JSMI a

    -- object . <selector>
    JS_Dot    :: (Sunroof a) => JSObject -> JSS a -> JSMI a

{-
    -- You can build functions, and pass them round
    JS_Function :: (Sunroof a, Sunroof b)
                => (a -> JSM b)
                -> JSM (JSFunction a b)
    -- You can invoke functions
    JS_Invoke :: JSFunction a b -> a -> JSM b
-}
    JS_Loop :: JSM () -> JSMI ()

-- Control.Monad.Operational makes a monad for us
type JSM a = Program JSMI a
-- we automatically get an  instance Monad JSM

-- export primitive instruction as part of the monad
jsSelect :: Sunroof a => JSS a -> JSM a
jsSelect jss = singleton $ JS_Select jss

-- compile an existing expression
compile :: Sunroof c => JSM c -> CompM (String,Type,Style)
compile = eval . view
    -- since the type  Program  is abstract (for efficiency),
    -- we have to apply the  view  function first,
    -- to get something we can pattern match on
    where
        eval :: Sunroof b => ProgramView JSMI b -> CompM (String,Type,Style)
        -- either we call a primitive JavaScript function
        eval (JS_Select jss :>>= g) = do
            code <- toC App.<$> compileJSS jss
            compileBind code g
        eval (JS_Dot o jss :>>= g) = do
            (sel_txt,ty,style) <- compileJSS jss
            let (o_txt,_,_) = directCompile o
            compileBind (toC ("(" ++ o_txt ++ ")." ++ sel_txt,ty,style)) g
        eval (JS_Loop inner :>>= g) = do
            (inner_txt,Unit,Continue) <- compile inner
            compileBind (toC ("function(){ var body = " ++ inner_txt ++ ";Y(body);}",Unit,Continue)) g

        -- or we're done already
        eval (Return b) = return $ directCompile b

compileBind :: (Sunroof a, Sunroof b) => (String,Type) -> (a -> JSM b) -> CompM (String,Type,Style)
compileBind (txt1,ty1) m2 = do
    a <- newVar
    (txt2,ty2) <- toC App.<$> compile (m2 a)
    case ty1 of
        Unit -> return ("function(k){(" ++ txt1 ++ ")(function(){(" ++ txt2 ++ ")(k)})}",ty2,Continue)
        _    -> return ("function(k){(" ++ txt1 ++ ")(function(" ++ showVar a ++ "){(" ++ txt2 ++ ")(k)})}",ty2,Continue)

-- convert Direct to CPS form
toC :: (String,Type,Style) -> (String,Type)
toC (txt,ty,style) =
    case (style,ty) of
        (Direct,Unit)  -> ("(function(k){" ++ txt ++ ";k();})",ty)
        (Direct,Value) -> ("(function(k){k(" ++ txt ++ ")})",ty)
        _ -> (txt,ty)

-- Most likely, the source code simplifies a lot
-- if you fuse the  compileBind  function into
-- the definition of the  eval  function.
-- What I did here is just a demonstration that the types will work out.
--
-- Also note that  CompM  does not really have to be a monad,
-- which may simplify compilation even further.

data JSS a where
    JSS_Call   :: String -> [JSValue] -> Type -> Style -> JSS a
    JSS_Select :: String ->              Type ->          JSS a
    (:=)       :: (Sunroof a) => JSF a -> a ->            JSS ()

data JSF a where
        JSF_Field  :: String -> JSF a

compileJSS :: (Sunroof a) => JSS a -> CompM (String,Type,Style)
compileJSS (JSS_Call nm args ty style) = do
        -- This show is doing the compile
        let inside = nm ++ "(" ++ intercalate "," (map show args) ++ ")"
        return (inside,ty,style)
compileJSS (JSS_Select nm ty) = do
        return (nm,ty,Direct)
compileJSS ((JSF_Field nm) := arg) = do
        return (nm ++ " = (" ++ show arg ++ ")",Unit,Direct)

uniqM :: CompM Uniq
uniqM = do
    n <- get
    modify (+1)
    return n

newVar :: (Sunroof a) => CompM a
newVar = mkVar App.<$> uniqM

type CompM a = State Uniq a

------------------------tests-----------------------
test2 :: JSM ()
test2 = jsSelect $ JSS_Call "foo" [cast (1 :: JSNumber)] Value Direct

run_test2 = compileJS test2

compileJS :: (Sunroof a) => JSM a -> (String,Type,Style)
compileJS = flip evalState 0 . compile
