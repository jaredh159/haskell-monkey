module Ast (
    Expr(..)
  , Program(..)
  , Stmt(..)
  , PrefixOp(..)
  , InfixOp(..)
  , Node(stringify)
  ) where

import Token as T

newtype Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt =
    LetStmt Token Expr
  | ReturnStmt Expr
  | ExprStmt Expr
  deriving (Eq, Show)

data InfixOp =
    InfixPlus
  | InfixMinus
  | InfixSlash
  | InfixAsterisk
  | InfixLt
  | InfixGt
  | InfixEq
  | InfixNotEq
  deriving (Eq, Show)

data PrefixOp =
    PrefixBang
  | PrefixMinus
  deriving (Eq, Show)

data Expr =
    IntLiteral Int
  | Prefix PrefixOp Expr
  | Infix Expr InfixOp Expr
  | Ident String
  deriving (Eq, Show)

-- Node

class Node a where
  stringify :: a -> String
  -- private
  s :: a -> String
  s = stringify

instance Node Program where
  stringify (Program stmts) = concatMap s stmts

instance Node Stmt where
  stringify (LetStmt (Tok _ t) expr) = "let " ++ t ++ " = " ++ s expr
  stringify (ReturnStmt expr) = "return " ++ s expr
  stringify (ExprStmt expr) = s expr

instance Node Expr where
  stringify (Prefix op expr) = "(" ++ lexeme op ++ s expr ++ ")"
  stringify (Infix lhs op rhs) = "(" ++ s lhs ++ " " ++ lexeme op ++ " " ++ s rhs ++ ")"
  stringify (Ast.Ident name) = name
  stringify (IntLiteral i) = show i

-- helpers

class Lexeme a where
  lexeme :: a -> String

instance Lexeme InfixOp where
  lexeme InfixPlus = "+"
  lexeme InfixMinus = "-"
  lexeme InfixSlash = "/"
  lexeme InfixAsterisk = "*"
  lexeme InfixLt = "<"
  lexeme InfixGt = ">"
  lexeme InfixEq = "=="
  lexeme InfixNotEq = "!="

instance Lexeme PrefixOp where
  lexeme PrefixBang = "!"
  lexeme PrefixMinus = "-"
