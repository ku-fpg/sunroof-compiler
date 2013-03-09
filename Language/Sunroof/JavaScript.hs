
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Sunroof.JavaScript
  ( Expr, ExprE(..), E(..)
  , Id, Stmt(..), Type(..)
  , showExpr, showStmt
  , operator, binOp, uniOp
  , literal
  ) where

import Data.List ( intercalate )
import Data.Reify ( MuRef(..) ) 
import Control.Applicative ( Applicative, pure, (<$>), (<*>))
import Data.Traversable ( Traversable(..) )
import Data.Foldable ( Foldable(..) )
import Data.Monoid ( Monoid(..) )
import Data.Char ( isAlpha )

-- -------------------------------------------------------------
-- Javascript Expressions
-- -------------------------------------------------------------

type Id = String

type Expr = E ExprE

data ExprE = ExprE Expr deriving Show

data E expr = Lit String    -- a precompiled (atomic) version of this literal
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

--instance Show Expr where
--  show = showExpr False

-- | Boolean argument says non-trivial arguments need parenthesis.
showExpr :: Bool -> Expr -> String
-- These being up here, cause a GHC warning for missing patterns.
-- So they are moved down.
--showExpr _ (Lit a) = a  -- always stand alone, or pre-parenthesised
--showExpr _ (Var v) = v  -- always stand alone
showExpr b e = p $ case e of
   (Lit a) -> a -- always stand alone, or pre-parenthesised
   (Var v) -> v -- always stand alone
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
            where args = [ "a" ++ show i | i <- take n ([0..] :: [Int])]
   -- This pattern was missing too.
   (Dot (ExprE _a) (ExprE _x) Unit) -> 
     error "Dot pattern on unit type. Don't know what to do."
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

-- -------------------------------------------------------------
-- Helper Combinators
-- -------------------------------------------------------------

-- | Combinator to create a operator/function applied to the given arguments.
operator :: Id -> [Expr] -> Expr
operator n ps = Apply (ExprE $ Var n) (fmap ExprE ps)

-- | Short-hand to create the applied binary operator/function.
--   See 'operator'.
binOp :: String -> Expr -> Expr -> E ExprE
binOp o e1 e2 = operator o [e1, e2]

-- | Short-hand to create the applied unary operator/function.
--   See 'operator'.
uniOp :: String -> Expr -> E ExprE
uniOp o e = operator o [e]

-- | Combinator to create a expression containing a 
--   literal in form of a string.
literal :: String -> Expr
literal = Lit


indent :: Int -> String -> String
indent n = unlines . map (take n (cycle "  ") ++) . lines

-- -------------------------------------------------------------
-- Javascript Statements
-- -------------------------------------------------------------

data Stmt = VarStmt Id Expr           -- var Id = Expr;   // Id is fresh
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

-- -------------------------------------------------------------
-- Javascript Types
-- -------------------------------------------------------------

data Type = Base         -- base type, like object
          | Unit
          | Fun Int      -- f (a_1,..,a_n), n == number in int
          deriving (Eq,Ord)

instance Show Type where
  show Base    = "*"
  show Unit    = "()"
  show (Fun n) = show n ++ " -> -"

-- -------------------------------------------------------------
-- Pretty Printer
-- -------------------------------------------------------------

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



