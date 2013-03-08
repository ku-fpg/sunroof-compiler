{-# LANGUAGE OverloadedStrings, GADTs, MultiParamTypeClasses, ScopedTypeVariables, RankNTypes, DataKinds, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- Syntax for JavaScript

module Language.Sunroof.JavaScript where

import Data.List ( intercalate )
import Data.Reify
import Control.Applicative ( Applicative, pure, (<$>), (<*>))
import Data.Traversable
import Data.Foldable hiding (all, any)
import Data.Semigroup
import Data.Char

type Id = String

type Expr = E ExprE

data ExprE = ExprE Expr
        deriving Show

---------------------------------------------------------------
-- Trivial expression language for Java
---------------------------------------------------------------

data E expr
        = Lit String    -- a precompiled (atomic) version of this literal
        | Var Id
        | Dot expr expr Type    -- expr . expr :: Type
        | Apply expr [expr]     -- expr ( expr, ..., expr )
        | Function [Id] [Stmt]
        deriving Show

instance MuRef ExprE where
  type DeRef ExprE = E
  mapDeRef f (ExprE e) = traverse f e


instance Traversable E where
  traverse _ (Lit s) = pure (Lit s)
  traverse _ (Var s) = pure (Var s)
  traverse f (Dot o n a) = Dot <$> f o <*> f n <*> pure a
  traverse f (Apply s xs) = Apply <$> f s <*> traverse f xs
  traverse _ (Function nms stmts) = pure (Function nms stmts)

instance Foldable E where
  foldMap _ (Lit _) = mempty
  foldMap _ (Var _) = mempty
  foldMap f (Dot o n _) = f o `mappend` f n
  foldMap f (Apply o xs) = f o `mappend` foldMap f xs
  foldMap _ (Function _nms _stmts) = mempty

instance Functor E where
  fmap _ (Lit s) = Lit s
  fmap _ (Var s) = Var s
  fmap f (Dot o n a) = Dot (f o) (f n) a
  fmap f (Apply s xs) = Apply (f s) (map f xs)
  fmap _ (Function nms stmts) = Function nms stmts

--
--instance Show Expr where
--        show = showExpr False

-- | Boolean argument says non-trivial arguments need parenthesis.

showExpr :: Bool -> Expr -> String
showExpr _ (Lit a) = a  -- always stand alone, or pre-parenthesised
showExpr _ (Var v) = v  -- always stand alone
showExpr b e = p $ case e of
--   (Apply (ExprE (Var "[]")) [ExprE a,ExprE x])   -> showExpr True a ++ "[" ++ showExpr False x ++ "]"
   (Apply (ExprE (Var "?:")) [ExprE a,ExprE x,ExprE y]) -> showExpr True a ++ "?" ++ showExpr True x ++ ":" ++ showExpr True y
   (Apply (ExprE (Var op)) [ExprE x,ExprE y]) | not (any isAlpha op) -> showExpr True x ++ op ++ showExpr True y
   (Apply (ExprE fn) args) -> showFun fn args
   (Dot (ExprE a) (ExprE x) Base) -> showIdx a x
        -- This is a shortcomming in Javascript, where grabbing a indirected function
        -- throws away the context (self/this). So we force storage of the context, using a closure.
   (Dot (ExprE a) (ExprE x) (Fun n)) ->
                "function(" ++ intercalate "," args ++ ") { return (" ++
                        showIdx a x ++ ")(" ++ intercalate "," args ++ "); }"
         where args = [ "a" ++ show i | i <- take n [0..]]
   (Function args body) ->
                "function" ++
                "(" ++ intercalate "," args ++ ") {\n" ++
                   indent 2 (unlines (map showStmt body)) ++
                "}"
 where
   p txt = if b then "(" ++ txt ++ ")" else txt

showIdx :: Expr -> Expr -> String
showIdx a x = showExpr True a ++ "[" ++ showExpr False x ++ "]"

-- Show a function argument,
showFun :: Expr -> [ExprE] -> String
showFun e args = case e of
    (Dot (ExprE a) (ExprE x) _) -> "(" ++ showIdx a x ++ ")" ++ args_text
    _                           -> showExpr True e ++ args_text
  where args_text = "(" ++ intercalate "," (map (\ (ExprE e') -> showExpr False e') args) ++ ")"

indent :: Int -> String -> String
indent n = unlines . map (take n (cycle "  ") ++) . lines

data Stmt
        = VarStmt Id Expr           -- var Id = Expr;   // Id is fresh
        | AssignStmt Expr Expr Expr -- Expr[Expr] = Expr
        | AssignStmt_ Expr Expr     -- Expr = Expr      // restrictions on lhs
        | ExprStmt Expr             -- Expr
        | ReturnStmt Expr           -- return Expr
        | IfStmt Expr [Stmt] [Stmt] -- if (Expr) { Stmts } else { Stmts }
        | WhileStmt Expr [Stmt]     -- while (Expr) { Stmts }

instance Show Stmt where
        show = showStmt

showStmt :: Stmt -> String
showStmt (VarStmt v e) | null v = showExpr False e ++ ";"
showStmt (VarStmt v e) = "var " ++ v ++ " = " ++ showExpr False e ++ ";"
showStmt (AssignStmt e1 e2 e3) = showExpr True e1 ++ "[" ++ showExpr False e2 ++ "] = " ++ showExpr False e3 ++ ";"
showStmt (AssignStmt_ e1 e2) = showExpr False e1 ++ " = " ++ showExpr False e2 ++ ";"
showStmt (ExprStmt e) = showExpr False e ++ ";"
showStmt (ReturnStmt e) = "return " ++ showExpr False e ++ ";"
showStmt (IfStmt i t e) = "if(" ++ showExpr False i ++ "){\n"
  ++ indent 2 (unlines (map showStmt t))
  ++ "} else {\n"
  ++ indent 2 (unlines (map showStmt e))
  ++ "}"
showStmt (WhileStmt b stmts) = "while(" ++ showExpr False b ++ "){\n"
  ++ indent 2 (unlines (map showStmt stmts))
  ++ "}"


data Type
 = Base         -- base type, like object
 | Unit
 | Fun Int      -- f (a_1,..,a_n), n == number in int
  deriving (Eq,Ord, Show)

{-
        show (Lit a)  = a
        show (Var v) = v
        show (Op "[]" [a,x]) = "(" ++ show a ++ ")[" ++ show x ++ "]"
        show (Op "?:" [a,x,y]) = "((" ++ show a ++ ")?(" ++ show x ++ "):(" ++ show y ++ "))"
--        show (Op "(,)" [x,y]) = "[" ++ show x ++ "," ++ show y ++ "]"
        show (Op x [a,b]) | all (not . isAlpha) x = "(" ++ show a ++ ")" ++ x ++ "(" ++ show b ++ ")"
        show (Op fn args) = fn ++ "(" ++ intercalate "," (map show args) ++ ")"
--        show (Cast e) = show e
-}
-- Trivial pretty printer

data Doc = Text String           -- plain text (assume no newlines)
         | Indent Int Doc        -- indent document by n
         | Sep [Doc]             -- on seperate lines

text :: String -> Doc
text = Text

--indent :: Int -> Doc -> Doc
--indent = Indent

sep :: [Doc] -> Doc
sep = Sep

pretty :: Doc -> String
pretty (Text txt) = txt
pretty (Sep docs) = unlines $ map pretty docs
pretty (Indent n doc) = unlines $ map (take n (cycle "  ") ++) $ lines $ pretty doc



