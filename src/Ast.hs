module Ast (
    Expr(..)
  , Program(..)
  , Stmt(..)
  , PrefixOp(..)
  , InfixOp(..)
  , Node(..)
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
    BoolLit Bool
  | IntLit Int
  | Prefix PrefixOp Expr
  | Infix Expr InfixOp Expr
  | Ident String
  | If { cond :: Expr, conseq :: [Stmt], alt :: Maybe [Stmt] }
  deriving (Eq, Show)

-- Node

class Node a where
  stringify :: a -> String

instance Node Program where
  stringify (Program stmts) = stringify stmts

instance (Node a) => Node [a] where
  stringify = concatMap stringify

instance Node Stmt where
  stringify (LetStmt (Tok _ t) expr) = "let " ++ t ++ " = " ++ s expr
  stringify (ReturnStmt expr) = "return " ++ s expr
  stringify (ExprStmt expr) = s expr

instance Node Expr where
  stringify (Prefix op expr) = "(" ++ lexeme op ++ s expr ++ ")"
  stringify (Infix lhs op rhs) = "(" ++ s lhs ++ " " ++ lexeme op ++ " " ++ s rhs ++ ")"
  stringify (Ast.Ident name) = name
  stringify (IntLit i) = show i
  stringify (BoolLit b) = if b then "true" else "false"
  stringify (Ast.If cond cons alt) = "if " ++ s cond ++ " " ++ s cons ++ (case alt of
    Nothing -> ""
    Just alt' -> "else " ++ s alt')

s :: (Node a) => a -> String
s = stringify

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
