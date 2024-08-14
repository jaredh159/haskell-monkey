module Eval (
    eval,
    evalWith,
    emptyEnv,
    Env(..)
  ) where

import Prelude hiding (negate)
import qualified Ast
import Object

import Control.Monad.State (State, foldM, evalState)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import qualified Data.Map as M

data Env = Env (M.Map String Object) (Maybe Env)
type EvalResult = ExceptT Error (State Env) Object
type Error = String

eval :: Ast.Node -> Either Error Object
eval node = evalState (runExceptT $ evalR node) emptyEnv

evalWith :: Ast.Node -> Env -> Either Error Object
evalWith node = evalState $ runExceptT $ evalR node

emptyEnv :: Env
emptyEnv = Env M.empty Nothing

evalR :: Ast.Node -> EvalResult
evalR (Ast.ProgNode stmts) = unwrapReturn <$> evalStmts stmts
evalR (Ast.BlockNode stmts) = evalStmts stmts
evalR (Ast.StmtNode stmt) = evalStmt stmt
evalR (Ast.ExprNode expr) = evalExpr expr

evalStmts :: [Ast.Stmt] -> EvalResult
evalStmts = foldM (\prev stmt -> case prev of
  obj@(ObjReturn _) -> return obj
  _ -> evalR (Ast.StmtNode stmt)) ObjNull

evalStmt :: Ast.Stmt -> EvalResult
evalStmt (Ast.ExprStmt expr) = evalExpr expr
evalStmt (Ast.ReturnStmt expr) =  ObjReturn <$> evalExpr expr
evalStmt stmt = throwE  $ "Unhandled statement type: " ++ show stmt

evalExpr :: Ast.Expr -> EvalResult
evalExpr (Ast.IntLit int) = return (ObjInt int)
evalExpr (Ast.BoolLit bool) = return (ObjBool bool)
evalExpr (Ast.Prefix Ast.PrefixBang expr) = negate <$> evalR (Ast.ExprNode expr)
evalExpr (Ast.Prefix Ast.PrefixMinus expr) = evalR (Ast.ExprNode expr) >>= f where
  f (ObjInt int) = return $ ObjInt (-int)
  f obj = throwE $ "Unknown operator: -" ++ objType obj
evalExpr (Ast.Infix lhs op rhs) = evalInfixExpr lhs op rhs
evalExpr (Ast.If cond cons alt) = do
  cond' <- evalR $ Ast.ExprNode cond
  case (truthy cond', alt) of
    (True, _) -> evalR $ Ast.BlockNode cons
    (False, Just alt') -> evalR $ Ast.BlockNode alt'
    _ -> return ObjNull
evalExpr expr = throwE $ "Unhandled expr type: " ++ show expr

evalInfixExpr :: Ast.Expr -> Ast.InfixOp -> Ast.Expr -> EvalResult
evalInfixExpr lhs op rhs = do
  lhs' <- evalExpr lhs
  rhs' <- evalExpr rhs
  case (lhs', op, rhs') of
     (ObjInt lval, Ast.InfixPlus, ObjInt rval) -> return $ ObjInt (lval + rval)
     (ObjInt lval, Ast.InfixAsterisk, ObjInt rval) -> return $ ObjInt (lval * rval)
     (ObjInt lval, Ast.InfixSlash, ObjInt rval) -> return $ ObjInt (lval `div` rval)
     (ObjInt lval, Ast.InfixLt, ObjInt rval) -> return $ ObjBool (lval < rval)
     (ObjInt lval, Ast.InfixGt, ObjInt rval) -> return $ ObjBool (lval > rval)
     (ObjInt lval, Ast.InfixEq, ObjInt rval) -> return $ ObjBool (lval == rval)
     (ObjInt lval, Ast.InfixNotEq, ObjInt rval) -> return $ ObjBool (lval /= rval)
     (ObjBool lval, Ast.InfixEq, ObjBool rval) -> return $ ObjBool (lval == rval)
     (ObjBool lval, Ast.InfixNotEq, ObjBool rval) -> return $ ObjBool (lval /= rval)
     _ -> throwE $ "Type mismatch: " ++ objType lhs' ++ " " ++ Ast.lexeme op ++ " " ++ objType rhs'

-- helpers

unwrapReturn :: Object -> Object
unwrapReturn (ObjReturn obj) = obj
unwrapReturn obj = obj

negate :: Object -> Object
negate (ObjBool bool) = ObjBool $ not bool
negate ObjNull = ObjBool True
negate _ = ObjBool False

truthy :: Object -> Bool
truthy ObjNull = False
truthy (ObjBool False) = False
truthy _ = True
