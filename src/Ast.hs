module Ast (Expr(..), Program(..), Stmt(..)) where

import Token

newtype Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt =
    LetStmt Token Expr
  | ReturnStmt Expr
  deriving (Eq, Show)

data Expr =
    Bool
  | Int
  deriving (Eq, Show)

data Node =
    ProgramNode Program
  | StmtNode Stmt
  | ExprNode Expr

