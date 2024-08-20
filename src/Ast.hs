module Ast
  ( Expr(..)
  , Stmt(..)
  , PrefixOp(..)
  , InfixOp(..)
  , Stringify(..)
  , Node(..)
  , Lexeme(..)
  , Program
  ) where

import Data.List (intercalate)
import qualified Data.Map as M

import Token as T

data Node =
    ProgNode [Stmt]
  | BlockNode [Stmt]
  | StmtNode Stmt
  | ExprNode Expr
  deriving (Eq, Show)

data Stmt =
    LetStmt String Expr
  | ReturnStmt Expr
  | ExprStmt Expr
  deriving (Eq, Show, Ord)

type Program = [Stmt]

data InfixOp =
    InfixPlus
  | InfixMinus
  | InfixSlash
  | InfixAsterisk
  | InfixLt
  | InfixGt
  | InfixEq
  | InfixNotEq
  deriving (Eq, Show, Ord)

data PrefixOp =
    PrefixBang
  | PrefixMinus
  deriving (Eq, Show, Ord)

data Expr =
    BoolLit Bool
  | IntLit Int
  | StringLit String
  | ArrayLit [Expr]
  | HashLit (M.Map Expr Expr)
  | Prefix PrefixOp Expr
  | Infix Expr InfixOp Expr
  | Index Expr Expr
  | Ident String
  | FnLit [String] [Stmt]
  | Call Expr [Expr]
  | If { cond :: Expr, conseq :: [Stmt], alt :: Maybe [Stmt] }
  deriving (Eq, Show, Ord)

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
  stringify (StringLit string) = string
  stringify (BoolLit b) = if b then "true" else "false"
  stringify (FnLit params body) = "fn(" ++ intercalate ", " params ++ ")" ++ s body
  stringify (Call fn args) = s fn ++ "(" ++ intercalate ", " (map s args) ++ ")"
  stringify (ArrayLit exprs) = "[" ++ intercalate ", " (map s exprs) ++ "]"
  stringify (Index lhs index) = "(" ++ s lhs ++ "[" ++ s index ++ "])"
  stringify (HashLit hashmap) =
    let entries = map (\(key, val) -> s key ++ ": " ++ s val) (M.toList hashmap) in
    "{" ++ intercalate ", " entries ++ "}"
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
