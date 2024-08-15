module Eval
  ( eval
  , evalWith
  ) where

import Prelude hiding (negate)
import qualified Ast
import Object
import qualified Env
import Env (Env)

import Control.Monad.State (State, foldM, evalState, runState)
import Control.Monad.State (MonadTrans (lift), MonadState (get), modify)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import qualified Data.Map as M

type EvalResult = ExceptT Error (State Env) Object
type Error = String

eval :: Ast.Node -> Either Error Object
eval node = evalState (runExceptT $ evalR node) Env.empty

evalWith :: Ast.Node -> Env -> (Either Error Object, Env)
evalWith node = runState $ runExceptT $ evalR node

evalR :: Ast.Node -> EvalResult
evalR (Ast.ProgNode stmts) = unwrapReturn <$> evalStmts stmts
evalR (Ast.BlockNode stmts) = evalStmts stmts
evalR (Ast.StmtNode stmt) = evalStmt stmt
evalR (Ast.ExprNode expr) = evalExpr expr

evalStmts :: [Ast.Stmt] -> EvalResult
evalStmts = foldM (\prev stmt -> case prev of
  wrapped@(ObjReturn _) -> return wrapped
  _ -> evalR (Ast.StmtNode stmt)) ObjNull

evalStmt :: Ast.Stmt -> EvalResult
evalStmt (Ast.ExprStmt expr) = evalExpr expr
evalStmt (Ast.ReturnStmt expr) =  ObjReturn <$> evalExpr expr
evalStmt (Ast.LetStmt ident expr) = do
  val <- evalExpr expr
  lift $ modify (Env.bind ident val)
  return ObjNull

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
evalExpr (Ast.Ident ident) = do
  env <- lift get
  case Env.get ident env of
    Just obj -> return obj
    Nothing -> throwE $ "Identifier not found: `" ++ ident ++ "`"
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
