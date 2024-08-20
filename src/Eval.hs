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
import GHC.IO (unsafePerformIO)

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
  _ -> evalStmt stmt) ObjNull

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
      "push" -> pure $ ObjBuiltIn BuiltInPush
      "puts" -> pure $ ObjBuiltIn BuiltInPuts
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
    indexArray items i
      | i < 0 || i >= length items = ObjNull
      | otherwise = items !! i
evalExpr (Ast.Call fnExpr argExprs) = do
  fn <- evalExpr fnExpr
  args <- mapM evalExpr argExprs
  case fn of
    (ObjFn params body outer) -> do
      when (length params /= length args) $ throwE $
        "Incorrect num args, expected: " ++ show (length params)
      env <- get
      let fnEnv = extendFnEnv (Env.chain env outer) params args
      liftEither $ unwrapReturn <$> evalIn (Ast.BlockNode body) fnEnv
    (ObjBuiltIn builtin) -> callBuiltIn builtin args
    obj -> throwE $ "Not a function: " ++ objType obj
evalExpr (Ast.HashLit _) = error "TODO: eval hash lit"

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

callBuiltIn :: Env.BuiltIn -> [Object] -> EvalResult
callBuiltIn builtin args = case builtin of
    BuiltInLen -> do
      case args of
        [ObjString string] -> pure $ ObjInt $ length string
        [ObjArray elems] -> pure $ ObjInt $ length elems
        _ -> builtInError args "Argument to `len` not supported" 1
    BuiltInFirst -> do
      case args of
        [ObjArray elems] -> pure $ if null elems then ObjNull else head elems
        _ -> builtInError args "Argument to `first` not supported" 1
    BuiltInLast -> do
      case args of
        [ObjArray elems] -> pure $ if null elems then ObjNull else last elems
        _ -> builtInError args "Argument to `last` not supported" 1
    BuiltInRest -> do
      case args of
        [ObjArray []] -> pure ObjNull
        [ObjArray objs] -> pure $ ObjArray $ tail objs
        _ -> builtInError args "Argument to `rest` not supported" 1
    BuiltInPush -> do
      case args of
        [ObjArray array, obj] -> pure $ ObjArray (array ++ [obj])
        _ -> builtInError args "Argument to `push` must be ARRAY" 2
    BuiltInPuts ->
      let output = unlines $ map Env.printObj args in
      unsafePerformIO (putStr output) `seq` pure ObjNull

builtInError :: [Object] -> String -> Int -> EvalResult
builtInError args msg arity
  | length args == arity = throwE $ msg ++ ", got " ++ objType (head args)
  | otherwise = throwE $
      "Wrong num args, expected " ++ show arity ++ ", got " ++ show (length args)

extendFnEnv :: Env -> [String] -> [Object] -> Env
extendFnEnv fnEnv params args =
  foldl
    (\env (ident, obj) -> Env.bind ident obj env)
    (Env.enclosed fnEnv) (zip params args)

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
