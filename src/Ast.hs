module Ast (
    Expr(..)
  , Program(..)
  , Stmt(..)
  , PrefixOp(..)
  ) where

import Token

newtype Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt =
    LetStmt Token Expr
  | ReturnStmt Expr
  | ExprStmt Expr
  deriving (Eq, Show)

data PrefixOp =
    PrefixBang
  | PrefixMinus
  deriving (Eq, Show)

data Expr =
    IntLiteral Int
  | Prefix PrefixOp Expr
  | Ident String
  deriving (Eq, Show)

data Node =
    ProgramNode Program
  | StmtNode Stmt
  | ExprNode Expr

