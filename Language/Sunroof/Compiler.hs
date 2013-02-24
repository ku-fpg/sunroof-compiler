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
          eval (JS_Eval e :>>= g) = do
            compileBind (unbox e) (JS . g)
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
          eval (JS_Function fun :>>= g) = do
            e <- compileFunction fun
            compileBind e (JS . g)
          eval (JS_Branch b c1 c2 :>>= g) = do
            branch <- compileBranch b c1 c2
            compileStatement branch (JS . g)
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

