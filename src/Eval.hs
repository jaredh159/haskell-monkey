module Eval (
    eval
  ) where

import qualified Ast
import Object

eval :: Ast.Node -> Object
eval (Ast.ProgNode prog) = foldl (\_ stmt -> eval (Ast.StmtNode stmt)) ObjNull prog
eval (Ast.StmtNode stmt) = evalStmt stmt
eval node = error $ "Node not handled: " ++ show node

evalStmt :: Ast.Stmt -> Object
evalStmt (Ast.ExprStmt expr) = evalExpr expr
evalStmt _ = error "unhandled stmt"

evalExpr :: Ast.Expr -> Object
evalExpr (Ast.IntLit int) = ObjInt int
evalExpr expr = error $ "unhandled expr type" ++ show expr
