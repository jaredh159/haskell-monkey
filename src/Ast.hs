module Ast (tokenLiteral, Expr(..), Program(..), Stmt(..)) where

import Token

newtype Program = Program [Stmt]
  deriving (Eq, Show)

-- data Ident = Ident Token String
--   deriving (Eq, Show)

data Stmt =
    LetStmt Token Token Expr
  | ReturnStmt
  deriving (Eq, Show)

data Expr =
    Bool
  | Int
  deriving (Eq, Show)

data Node =
    ProgramNode Program
  | StmtNode Stmt
  | ExprNode Expr

tokenLiteral :: Node -> String
tokenLiteral (ProgramNode (Program [])) = ""
tokenLiteral (ProgramNode (Program (st:_))) = tokenLiteral $ StmtNode st
tokenLiteral (StmtNode (LetStmt (T _ literal) _ _)) = literal
tokenLiteral (StmtNode ReturnStmt) = undefined
tokenLiteral (ExprNode _) = undefined

-- pub trait Node {
  -- fn token_literal(&self) -> String;
  -- fn string(&self) -> String;
-- }

-- pub enum Stmt {
  -- Let(Token, Identifier, Expr),
  -- Return(Token, Expr),
  -- Expr(Token, Expr),
  -- Block(BlockStmt),
-- }

-- pub enum Expr {
  -- Array(ArrayLiteral),
  -- Bool(BooleanLiteral),
  -- Call(CallExpr),
  -- Func(FunctionLiteral),
  -- Hash(HashLiteral),
  -- ... more
-- }

