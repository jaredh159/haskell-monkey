module Ast (Expr(..), Program(..), Stmt(..)) where

import Token

newtype Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt =
    LetStmt Token Expr
  | ReturnStmt Expr
  | ExprStmt Expr
  deriving (Eq, Show)

data Expr =
    IntLiteral Int
  | Identifier String
  deriving (Eq, Show)

data Node =
    ProgramNode Program
  | StmtNode Stmt
  | ExprNode Expr

