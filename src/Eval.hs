module Eval
  ( eval
  , evalS
  ) where

import Prelude hiding (negate)
import qualified Ast
import qualified Env
import Env (Env, Object(..), BuiltIn(..), objType)

import Control.Monad.State (State, foldM, evalState, runState, when)
import Control.Monad.State (MonadTrans (lift), MonadState (get), modify)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import qualified Data.Map as M
import Control.Monad.Error.Class (liftEither)

type EvalResult = ExceptT Error (State Env) Object
type Error = String

eval :: Ast.Node -> Either Error Object
eval node = evalIn node Env.empty

evalIn :: Ast.Node -> Env -> Either Error Object
evalIn node = evalState (runExceptT $ evalR node)

evalS :: Ast.Node -> Env -> (Either Error Object, Env)
evalS node = runState $ runExceptT $ evalR node

evalR :: Ast.Node -> EvalResult
evalR (Ast.ProgNode stmts) = unwrapReturn <$> evalStmts stmts
evalR (Ast.BlockNode stmts) = evalStmts stmts
evalR (Ast.StmtNode stmt) = evalStmt stmt
evalR (Ast.ExprNode expr) = evalExpr expr

evalStmts :: [Ast.Stmt] -> EvalResult
evalStmts = foldM (\prev stmt -> case prev of
  wrapped@(ObjReturn _) -> pure wrapped
  _ -> evalR (Ast.StmtNode stmt)) ObjNull

evalStmt :: Ast.Stmt -> EvalResult
evalStmt (Ast.ExprStmt expr) = evalExpr expr
evalStmt (Ast.ReturnStmt expr) =  ObjReturn <$> evalExpr expr
evalStmt (Ast.LetStmt ident expr) = do
  val <- evalExpr expr
  lift $ modify (Env.bind ident val)
  pure ObjNull

evalExpr :: Ast.Expr -> EvalResult
evalExpr (Ast.IntLit int) = pure (ObjInt int)
evalExpr (Ast.BoolLit bool) = pure (ObjBool bool)
evalExpr (Ast.StringLit string) = pure (ObjString string)
evalExpr (Ast.Prefix Ast.PrefixBang expr) = negate <$> evalExpr expr
evalExpr (Ast.Prefix Ast.PrefixMinus expr) = evalExpr expr >>= f where
  f (ObjInt int) = pure $ ObjInt (-int)
  f obj = throwE $ "Unknown operator: -" ++ objType obj
evalExpr (Ast.Infix lhs op rhs) = evalInfixExpr lhs op rhs
evalExpr (Ast.If cond cons alt) = do
  cond' <- evalExpr cond
  case (truthy cond', alt) of
    (True, _) -> evalR $ Ast.BlockNode cons
    (False, Just alt') -> evalR $ Ast.BlockNode alt'
    _ -> pure ObjNull
evalExpr (Ast.Ident ident) = do
  env <- lift get
  case Env.get ident env of
    Just obj -> pure obj
    Nothing -> case ident of
      "len" -> pure $ ObjBuiltIn BuiltInLen
      "first" -> pure $ ObjBuiltIn BuiltInFirst
      "last" -> pure $ ObjBuiltIn BuiltInLast
      "rest" -> pure $ ObjBuiltIn BuiltInRest
      _ -> throwE $ "Identifier not found: `" ++ ident ++ "`"
evalExpr (Ast.FnLit params body) = do
  env <- lift get
  pure $ ObjFn params body env
evalExpr (Ast.ArrayLit exprs) = ObjArray <$> mapM evalExpr exprs
evalExpr (Ast.Index lhs idx) = do
  lhs' <- evalExpr lhs
  idx' <- evalExpr idx
  case (lhs', idx') of
    (ObjArray elems, ObjInt i) -> pure $ indexArray elems i
    (lhs'', idx'') -> throwE $
      "Invalid index expr: " ++ objType lhs'' ++ "[" ++ objType idx'' ++ "]"
  where
    indexArray items i'
      | i' < 0 || i' >= length items = ObjNull
      | otherwise = items !! i'
evalExpr (Ast.Call fn args) = do
  fn' <- evalExpr fn
  case fn' of
    (ObjFn params body outer) -> do
      when (length params /= length args) $ throwE $
        "Incorrect num args, expected: " ++ show (length params)
      args' <- mapM evalExpr args
      let fnEnv = extendFnEnv outer params args'
      liftEither $ evalIn (Ast.BlockNode body) fnEnv
    (ObjBuiltIn BuiltInLen) -> do
      args' <- mapM evalExpr args
      case args' of
        [ObjString string] -> pure $ ObjInt $ length string
        [ObjArray elems] -> pure $ ObjInt $ length elems
        [obj] -> throwE $ "Argument to `len` not supported, got " ++ objType obj
        objs -> throwE $ "Wrong num args, expected 1, got " ++ show (length objs)
    (ObjBuiltIn BuiltInFirst) -> do
      args' <- mapM evalExpr args
      case args' of
        [ObjArray elems] -> pure $ if null elems then ObjNull else head elems
        [obj] -> throwE $ "Argument to `first` not supported, got " ++ objType obj
        objs -> throwE $ "Wrong num args, expected 1, got " ++ show (length objs)
    (ObjBuiltIn BuiltInLast) -> do
      args' <- mapM evalExpr args
      case args' of
        [ObjArray elems] -> pure $ if null elems then ObjNull else last elems
        [obj] -> throwE $ "Argument to `last` not supported, got " ++ objType obj
        objs -> throwE $ "Wrong num args, expected 1, got " ++ show (length objs)
    (ObjBuiltIn BuiltInRest) -> do
      args' <- mapM evalExpr args
      case args' of
        [ObjArray []] -> pure ObjNull
        [ObjArray objs] -> pure $ ObjArray $ tail objs
        [obj] -> throwE $ "Argument to `rest` not supported, got " ++ objType obj
        objs -> throwE $ "Wrong num args, expected 1, got " ++ show (length objs)
    obj -> throwE $ "Not a function: " ++ objType obj

extendFnEnv :: Env -> [String] -> [Object] -> Env
extendFnEnv fnEnv params args = foldl
  (\env (ident, obj) -> Env.bind ident obj env)
  (Env.enclosed fnEnv) (zip params args)

evalInfixExpr :: Ast.Expr -> Ast.InfixOp -> Ast.Expr -> EvalResult
evalInfixExpr lhs op rhs = do
  lhs' <- evalExpr lhs
  rhs' <- evalExpr rhs
  case (lhs', op, rhs') of
    (ObjInt lval, Ast.InfixPlus, ObjInt rval) -> pure $ ObjInt (lval + rval)
    (ObjString lval, Ast.InfixPlus, ObjString rval) -> pure $ ObjString (lval ++ rval)
    (ObjInt lval, Ast.InfixMinus, ObjInt rval) -> pure $ ObjInt (lval - rval)
    (ObjInt lval, Ast.InfixAsterisk, ObjInt rval) -> pure $ ObjInt (lval * rval)
    (ObjInt lval, Ast.InfixSlash, ObjInt rval) -> pure $ ObjInt (lval `div` rval)
    (ObjInt lval, Ast.InfixLt, ObjInt rval) -> pure $ ObjBool (lval < rval)
    (ObjInt lval, Ast.InfixGt, ObjInt rval) -> pure $ ObjBool (lval > rval)
    (ObjInt lval, Ast.InfixEq, ObjInt rval) -> pure $ ObjBool (lval == rval)
    (ObjInt lval, Ast.InfixNotEq, ObjInt rval) -> pure $ ObjBool (lval /= rval)
    (ObjBool lval, Ast.InfixEq, ObjBool rval) -> pure $ ObjBool (lval == rval)
    (ObjBool lval, Ast.InfixNotEq, ObjBool rval) -> pure $ ObjBool (lval /= rval)
    (ObjString _, Ast.InfixMinus, ObjString _) -> throwE "Unknown operator: STRING - STRING"
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
