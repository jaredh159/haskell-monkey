module Eval (
    eval
  ) where

import Prelude hiding (negate)
import qualified Ast
import Object

eval :: Ast.Node -> Object
eval (Ast.ProgNode []) = ObjNull
eval (Ast.ProgNode prog) = last $ map (eval . Ast.StmtNode) prog
eval (Ast.StmtNode stmt) = evalStmt stmt
eval (Ast.ExprNode expr) = evalExpr expr

evalStmt :: Ast.Stmt -> Object
evalStmt (Ast.ExprStmt expr) = evalExpr expr
evalStmt stmt = error  $ "Unhandled statement type: " ++ show stmt

evalExpr :: Ast.Expr -> Object
evalExpr (Ast.IntLit int) = ObjInt int
evalExpr (Ast.BoolLit bool) = ObjBool bool
evalExpr (Ast.Prefix Ast.PrefixBang expr) = negate $ eval $ Ast.ExprNode expr
evalExpr (Ast.Prefix Ast.PrefixMinus expr) = case eval (Ast.ExprNode expr) of
  (ObjInt int) -> ObjInt (-int)
  _ -> ObjNull -- TODO: error
evalExpr expr = error $ "Unhandled expr type: " ++ show expr

negate :: Object -> Object
negate (ObjBool bool) = ObjBool $ not bool
negate ObjNull = ObjBool True
negate _ = ObjBool False



