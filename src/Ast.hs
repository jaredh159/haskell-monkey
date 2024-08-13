module Ast (
    Expr(..)
  , Stmt(..)
  , PrefixOp(..)
  , InfixOp(..)
  , Stringify(..)
  , Node(..)
  ) where

import Token as T
import Data.List (intercalate)

data Node =
    StmtsNode [Stmt]
  | StmtNode Stmt
  | ExprNode Expr
  deriving (Eq, Show)

data Stmt =
    LetStmt String Expr
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
  | FnLit [String] [Stmt]
  | Call Expr [Expr]
  | If { cond :: Expr, conseq :: [Stmt], alt :: Maybe [Stmt] }
  deriving (Eq, Show)

-- Stringify

class Stringify a where
  stringify :: a -> String

instance (Stringify a) => Stringify [a] where
  stringify = concatMap stringify

instance Stringify Stmt where
  stringify (LetStmt ident expr) = "let " ++ ident ++ " = " ++ s expr
  stringify (ReturnStmt expr) = "return " ++ s expr
  stringify (ExprStmt expr) = s expr

instance Stringify Expr where
  stringify (Prefix op expr) = "(" ++ lexeme op ++ s expr ++ ")"
  stringify (Infix lhs op rhs) = "(" ++ s lhs ++ " " ++ lexeme op ++ " " ++ s rhs ++ ")"
  stringify (Ast.Ident name) = name
  stringify (IntLit i) = show i
  stringify (BoolLit b) = if b then "true" else "false"
  stringify (FnLit params body) = "fn(" ++ intercalate ", " params ++ ")" ++ s body
  stringify (Call fn args) = s fn ++ "(" ++ intercalate ", " (map s args) ++ ")"
  stringify expr@(Ast.If {}) =
    "if " ++ s (cond expr) ++ " " ++ s (conseq expr) ++ (case alt expr of
      Nothing -> ""
      Just altExpr -> "else " ++ s altExpr)

s :: (Stringify a) => a -> String
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
