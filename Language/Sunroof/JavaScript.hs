
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

-- TODO: Remove as soon as the pretty printing stuff is actually used.
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- | Basic low-level types and their combinators.
--   These are used as output of the compiler.
--   Everything here is untypes and not supposed for public use!
module Language.Sunroof.JavaScript
  ( Expr, ExprE(..), E(..)
  , Id, Stmt(..), Type(..)
  , Rhs(..)
  , showExpr, showStmt
  , operator, binOp, uniOp
  , literal
  , scopeForEffect
  ) where

import Data.List ( intercalate )
import Data.Reify ( MuRef(..) )
import Control.Applicative ( Applicative, pure, (<$>), (<*>))
import Data.Traversable ( Traversable(..) )
import Data.Foldable ( Foldable(..) )
import Data.Monoid ( Monoid(..) )
import Data.Char ( isAlpha, isAlphaNum )

-- -------------------------------------------------------------
-- Javascript Expressions
-- -------------------------------------------------------------

-- | Javascript identifier.
type Id = String

-- | Short name for instantiated expressions.
type Expr = E ExprE

-- | Instantiated expressions.
data ExprE = ExprE Expr deriving Show

-- | Plain expressions in Javascript.
data E expr = Lit String -- ^ A precompiled (atomic) Javascript literal.
            | Var Id     -- ^ A variable.
            | Dot expr expr Type   -- ^ Field/attribute access (with type information): @expr . expr :: Type@
            | Apply expr [expr]    -- ^ Function application: @expr ( expr, ..., expr )@
            | Function [Id] [Stmt] -- ^ Anonymous function with parameter names and body.
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

-- | Show an expression as compiled Javascript.
--   The boolean argument says non-trivial arguments need parenthesis.
showExpr :: Bool -> Expr -> String
-- Original comment: These being up here, cause a GHC warning for missing patterns.
--                   So they are moved down.
-- Response: They *need* to be here, it makes a different. I've fixed the warning.
showExpr _ (Lit a) = a  -- always stand alone, or pre-parenthesised
showExpr _ (Var v) = v  -- always stand alone
showExpr b e = p $ case e of
--    (Apply (ExprE (Var "[]")) [ExprE a,ExprE x])   -> showExpr True a ++ "[" ++ showExpr False x ++ "]"
    (Apply (ExprE (Var "?:")) [ExprE a,ExprE x,ExprE y]) -> showExpr True a ++ "?" ++ showExpr True x ++ ":" ++ showExpr True y
    (Apply (ExprE (Var op)) [ExprE x,ExprE y]) | not (any isAlpha op) -> showExpr True x ++ op ++ showExpr True y
    (Apply (ExprE (Var "!")) [ExprE ex]) -> "!" ++ showExpr True ex
    -- We have a constructor call:
    (Apply (ExprE (Lit op)) args) | isNewConstructor op -> op ++ showArgs args
    (Apply (ExprE fn) args) -> showFun fn args
    (Dot (ExprE a) (ExprE x) Base) -> showIdx a x
    -- This is a shortcomming in Javascript, where grabbing a indirected function
    -- throws away the context (self/this). So we force storage of the context, using a closure.
    (Dot (ExprE a) (ExprE x) (Fun xs _)) ->
        "function(" ++ intercalate "," args ++ ") { return (" ++
          showIdx a x ++ ")(" ++ intercalate "," args ++ "); }"
      where args = [ "a" ++ show i | i <- take (length xs) ([0..] :: [Int])]
    -- This pattern was missing too.
    (Dot (ExprE _a) (ExprE _x) Unit) ->
      error "Dot pattern on unit type. Don't know what to do."
    (Function args body) ->
      "function" ++
      "(" ++ intercalate "," args ++ ") {\n" ++
         indent 2 (unlines (map showStmt body)) ++
      "}"
    _ -> error "should never happen"
  where
    p txt = if b then "(" ++ txt ++ ")" else txt

-- | @showIdx o a@ accesses the field/attribute @a@ of the object @o@.
showIdx :: Expr -> Expr -> String
showIdx a (Lit x) | Just n <- isGoodSelectName x
                  = showExpr True a ++ "." ++ n
showIdx a ix = showExpr True a ++ "[" ++ showExpr False ix ++ "]"

-- | @showArgs a@ creates a string representing the given expressions
--   in an argument list that can be used for functions or constructors.
showArgs :: [ExprE] -> String
showArgs args = "(" ++ intercalate "," (map (\ (ExprE e') -> showExpr False e') args) ++ ")"

-- | Show a function application,
showFun :: Expr -> [ExprE] -> String
showFun e args = case e of
    (Dot (ExprE a) (ExprE (Lit x)) _)
        | Just n <- isGoodSelectName x -> showExpr True a ++ "." ++ n ++ showArgs args
    (Dot (ExprE a) (ExprE x) _) -> "(" ++ showIdx a x ++ ")" ++ showArgs args
    _                           -> showExpr True e ++ showArgs args

-- | Check if the given 'Id' is a valid Javascript identifier.
isIdentifier :: Id -> Bool
isIdentifier x | not (null x) = isAlpha (head x) && all isAlphaNum (drop 1 x)
isIdentifier _ = False

-- | Check if the given 'Id' represents a constructor call without
--   arguments. That means a string beginning with @"new "@ followed
--   by a valid identifier ('isIdentifier').
isNewConstructor :: Id -> Bool
isNewConstructor x = take 4 x == "new " && isIdentifier (drop 4 x)

-- | Check if the given name is a field/attribute seletor that
--   can be printed without quotes using the dot-notation.
isGoodSelectName :: Id -> Maybe Id
isGoodSelectName xs
        | length xs < 2 = Nothing
        | head xs == '"' &&
          last xs == '"' &&
          all isAlpha xs' = return xs'
        | otherwise = Nothing
  where
          xs' = tail (init xs)

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

-- | Indent all lines of the given string by the given number
--   of spaces.
indent :: Int -> String -> String
indent n = unlines . map (take n (cycle "  ") ++) . lines

-- | Create a anonymous function to scope all effects
--   in the given block of statement.
scopeForEffect :: [Stmt] -> Expr
scopeForEffect stmts = Apply (ExprE $ Function [] stmts) []

-- -------------------------------------------------------------
-- Javascript References
-- -------------------------------------------------------------

-- | A Right hand side of an assignment.

data Rhs = VarRhs Id                  -- ^ A variable
         | DotRhs Expr Expr           -- ^ a named field

showRhs :: Rhs -> String
showRhs (VarRhs var)   = "var " ++ var
showRhs (DotRhs e1 e2) = showIdx e1 e2

-- -------------------------------------------------------------
-- Javascript Statements
-- -------------------------------------------------------------

-- TODO: remove VarStmt, replace with AssignStmt and ExprStmt.
-- TODO: add type to return stmt; should not return "null"

-- | Plain Javascript statements.
data Stmt = AssignStmt Rhs Expr       -- ^ Restricted assignment: @Rhs = Expr;@
          | DeleteStmt Expr           -- ^ Delete reference @delete Rhs;@
          | ExprStmt Expr             -- ^ Expression statement, for the sake of its side effects: @Expr;@
          | ReturnStmt Expr           -- ^ Return statement: @return Expr;@
          | IfStmt Expr [Stmt] [Stmt] -- ^ If-Then-Else statement: @if (Expr) { Stmts } else { Stmts }@
          | WhileStmt Expr [Stmt]     -- ^ While loop: @while (Expr) { Stmts }@
          | CommentStmt String        -- ^ A comment in the code: @// String@

instance Show Stmt where
  show = showStmt

-- | Translate a statement into actual Javascript.
showStmt :: Stmt -> String
showStmt (AssignStmt e1 e2) = showRhs e1 ++ " = " ++ showExpr False e2 ++ ";"
showStmt (DeleteStmt e) = "delete " ++ showExpr False e ++ ";"
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
showStmt (CommentStmt msg) = "/* " ++ msg ++ " */"

-- -------------------------------------------------------------
-- Javascript Types
-- -------------------------------------------------------------

-- | Abstract types for Javascript expressions in Sunroof.
data Type = Base -- ^ Base type like object or other primtive types.
          | Unit -- ^ Unit or void type. There is a effect but no value.
          | Fun [Type] Type -- ^ Function type: @(t_1,..,t_n) -> t@
          deriving (Eq,Ord)

instance Show Type where
  show Base    = "*"
  show Unit    = "()"
  show (Fun xs t) = show xs ++ " -> " ++ show t

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




