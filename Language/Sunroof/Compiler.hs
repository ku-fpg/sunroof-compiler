{-# LANGUAGE GADTs, RankNTypes, KindSignatures, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
module Language.Sunroof.Compiler
  ( compileJS, compileJS'
  ) where

import Data.Proxy

--import qualified Control.Applicative as App
import Control.Monad.Operational
import Control.Monad.State
--import Data.List (intercalate)

import Language.Sunroof.Types
--import Web.KansasComet (Template(..), extract)

compileAST :: (Sunroof a) => Uniq -> JS a -> (([Stmt], Expr), Uniq)
compileAST uq jsm = runState (compile jsm) uq

compileJS :: (Sunroof a) => JS a -> (String,String)
compileJS = fst . compileJS' 0

compileJS' :: (Sunroof a) => Uniq -> JS a -> ((String, String), Uniq)
compileJS' uq jsm = let ((stmts, res), u) = compileAST uq jsm
                    in ((unlines $ fmap show stmts, show res), u)

-- compile an existing expression
compile :: Sunroof c => JS c -> CompM ([Stmt], Expr)
compile = eval . view . unJS
    -- since the type  Program  is abstract (for efficiency),
    -- we have to apply the  view  function first,
    -- to get something we can pattern match on
    where eval :: Sunroof b => ProgramView JSI b -> CompM ([Stmt], Expr)
          -- either we call a primitive JavaScript function
--          eval (JS_Select jss :>>= g) = do
--            txt1 <- compileJSS jss
--            compileBind txt1 g
--          eval (JS_Dot o jss :>>= g) = do
--            sel_txt <- compileJSS jss
--            compileBind ("(" ++ showVar o ++ ")." ++ sel_txt) g

--          eval (JS_Eval e :>>= g) = do
--            compileBind (unbox e) (JS . g)
          eval (JS_Assign (JSSelector sel) a obj :>>= g) = do
            -- note, this is where we need to optimize/CSE  the a value.
            compileStatement ( AssignStmt (unbox obj) (unbox sel) (unbox a)
                             , Op "[]" [ExprE $ unbox obj, ExprE $ unbox sel] )
                             (JS . g)
          eval (JS_Select (JSSelector sel) obj :>>= g) = do
            compileBind (Op "[]" [ExprE $ unbox obj, ExprE $ unbox sel]) (JS . g)
          eval (JS_Invoke args fn :>>= g) = do
            -- Do we need paranthesis for the function? (showExpr True $ unbox fn)
            compileBind (Op (showVar fn) (map ExprE args)) (JS . g)

{-
compileAction o (Invoke args) =
        return ("","(" ++ showVar o ++ ")(" ++ intercalate "," (map show args) ++ ")")
compileAction o (JSSelector nm := val) =
                                -- this is a total hack, (pres. is wrong), but works
        return ("","(" ++ showVar o ++ ")[" ++ show nm ++ "] = (" ++ show (unbox val) ++ ")")


          eval (JS_App o jss :>>= g) = do
            (pre,act) <- compileAction o jss
            compileBind pre act (JS . g)

    JS_Assign  :: (Sunroof a) => JSSelector a -> a -> JSObject  -> JSI ()

    JS_Select  :: (Sunroof a) => JSSelector a -> JSObject      -> JSI a

    JS_Invoke :: (Sunroof r) => [Expr] -> JSFunction a r        -> JSI r        -- Perhaps take the overloaded vs [Expr]
                                                                                -- and use jsArgs in the compiler?


-}
--            ("(" ++ showVar o ++ ")" ++ sel_txt) g
--          eval (JS_Invoke args :>>= g) = do
--            compileBind ("(function(o) { return o.(" ++ intercalate "," (map show args) ++ ");}") g

          eval (JS_Function fun :>>= g) = do
            e <- compileFunction fun
            compileBind e (JS . g)
          eval (JS_Branch b c1 c2 :>>= g) = do
            branch <- compileBranch b c1 c2
            compileStatement branch (JS . g)
{-
          eval (JS_Loop body :>>= g) = do -- note, we do nothing with g, as it's unreachable
            -- create a new name for our loop
            loop <- newLoop
            -- the magic: add call to loop at end of loop body instructions,
            -- this way, if body contains a JS_Wait, it gets sucked into the
            -- event callback!
            loop_body <- compile (body >>= (\() -> singleton (JS_App (box $ Lit loop) $ Invoke [] :: JSI ())))
            -- define loop function, and call it once
            return ("var " ++ loop ++ " = function(){ " ++ loop_body ++ " }; " ++ loop ++ "();", "ERROR")
-}
{-
          eval (JS_Wait tmpl k :>>= g) = do
            txt1 <- compileFunction fun
            a <- newVar
            txt2 <- compile (g a)
            let eventNames = map fst $ extract tmpl
            return $ "$.kc.waitFor(" ++ show eventNames ++ ",function(" ++ showVar a ++ "){" ++ txt2 ++ "})"
-}
          -- or we're done already
          eval (Return b) = return ([], unbox b)

compileBind :: forall a b . (Sunroof a, Sunroof b)
            => Expr -> (a -> JS b) -> CompM ([Stmt], Expr)
compileBind e m2 = do
    a <- newVar
    (stmts,ret) <- compile (m2 a)
    return (assignVar (Proxy::Proxy a) (varId a) e : stmts , ret)

compileStatement :: (Sunroof a, Sunroof b)
                 => (Stmt, Expr) -> (a -> JS b) -> CompM ([Stmt], Expr)
compileStatement (stmt, e) m2 = do
    (stmts,ret) <- compile $ m2 (box e)
    return (stmt : stmts , ret)

{-
-- Does the same as 'compileBind' but does not bind the result of the passed in
-- JavaScript source to a variable. Like this control flow constructs like
-- branches can be translated.
compileCommand :: (Sunroof a, Sunroof b) => (String, a) -> (a -> JS b) -> CompM (String, String)
compileCommand (src1, comRes) f = do
    (src2,ret) <- compile (f comRes)
    return (src1 ++ ";" ++ src2, ret)
-}

compileBranch :: forall a bool . (Sunroof a, Sunroof bool)
              => bool -> JS a -> JS a -> CompM (Stmt, Expr)
compileBranch b c1 c2 = do
  (res :: a) <- newVar
  (src1, res1) <- compile c1
  (src2, res2) <- compile c2
  return ( IfStmt (unbox b) (src1 ++ [assignVar (Proxy::Proxy a) (varId res) res1])
                            (src2 ++ [assignVar (Proxy::Proxy a) (varId res) res2])
         , unbox res)

compileFunction :: forall a b . (JSArgument a, Sunroof b)
                => (a -> JS b) -> CompM Expr
compileFunction m2 = do
    (arg :: a) <- jsValue
    (fStmts,ret) <- compile (m2 arg)
    return $ Function (map varIdE $ jsArgs arg) (fStmts ++ [ReturnStmt ret])

-- These are a mix of properties, methods, and assignment.
-- What is the unifing name? JSProperty?
{-
compileAction :: (Sunroof a, Sunroof b) => a -> Action a b -> CompM (String,String)
compileAction o (Invoke args) =
        return ("","(" ++ showVar o ++ ")(" ++ intercalate "," (map show args) ++ ")")
compileAction o (JSSelector nm := val) =
                                -- this is a total hack, (pres. is wrong), but works
        return ("","(" ++ showVar o ++ ")[" ++ show nm ++ "] = (" ++ show (unbox val) ++ ")")
compileAction o (Map f act) =
        compileAction (f o) act
compileAction o (NoAction b) =
        return ("",showVar b)
compileAction o (BindAction (NoAction a) k) = compileAction o (k a)
compileAction o (BindAction m@(Invoke {}) k) = compileAction' o m k
compileAction o (BindAction m@(_ := _) k) = compileAction' o m k
compileAction o (BindAction m@(Map {}) k) = compileAction' o m k
compileAction o (BindAction (BindAction m k1) k2) = compileAction o (BindAction m (\ a -> BindAction (k1 a) k2))

compileAction' :: (Sunroof a, Sunroof b, Sunroof c) => a -> Action a b -> (b -> Action a c) => CompM (String,String)
compileAction' o m k = do
        (p1,v1) <- compileAction o m
        a <- newVar
        (p2,v2) <- compileAction o (k a)
        return (p1 ++ ";" ++ assignVar a ++ v1 ++ ";" ++ p2, v2)

--        return $ showVar b
--   BindAction :: Action a b -> (b -> Action a c)                -> Action a c
-}
{-
compileAction o (Multi many) = do
        patches <- mapM (compileAction o) many
        return $ intercalate ";" patches
-}

type CompM = State Uniq

instance UniqM CompM where
  uniqM = do
    n <- get
    modify (+1)
    return n

newVar :: (Sunroof a) => CompM a
newVar = jsVar

varId :: Sunroof a => a -> Id
varId = varIdE . unbox

varIdE :: Expr -> Id
varIdE e = case e of
  (Var v) -> v
  v -> error $ "varId: Expressions is not a variable: " ++ show v

{-
newLoop :: CompM String
newLoop = do
    u <- uniqM
    return $ "loop" ++ show u
-}
