module Eval (
    eval
  ) where

import Prelude hiding (negate)
import qualified Ast
import Object

type Error = String

eval :: Ast.Node -> Either Error Object
eval (Ast.ProgNode []) = Right ObjNull
eval (Ast.StmtNode stmt) = evalStmt stmt
eval (Ast.ExprNode expr) = evalExpr expr
eval (Ast.ProgNode prog) = foldl (\prev stmt -> case prev of
  Left _ -> prev
  Right _ -> eval (Ast.StmtNode stmt)) (Right ObjNull) prog

evalStmt :: Ast.Stmt -> Either Error Object
evalStmt (Ast.ExprStmt expr) = evalExpr expr
evalStmt stmt = error  $ "Unhandled statement type: " ++ show stmt

evalExpr :: Ast.Expr -> Either Error Object
evalExpr (Ast.IntLit int) = Right (ObjInt int)
evalExpr (Ast.BoolLit bool) = Right (ObjBool bool)
evalExpr (Ast.Prefix Ast.PrefixBang expr) = negate <$> eval (Ast.ExprNode expr)
evalExpr (Ast.Prefix Ast.PrefixMinus expr) = eval (Ast.ExprNode expr) >>= f where
  f (ObjInt int) = Right (ObjInt (-int))
  f _ = Left "Invalid rhs for PrefixMinus expression"
evalExpr expr = error $ "Unhandled expr type: " ++ show expr

negate :: Object -> Object
negate (ObjBool bool) = ObjBool $ not bool
negate ObjNull = ObjBool True
negate _ = ObjBool False



