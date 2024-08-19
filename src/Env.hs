module Env
  ( Env
  , empty
  , enclosed
  , bind
  , get
  , BuiltIn(..)
  , Object(..)
  , objType
  , chain
  ) where

import qualified Data.Map as M
import qualified Ast
import Control.Applicative ((<|>))

data Env = E (M.Map String Object) (Maybe Env) deriving (Eq, Show)

empty :: Env
empty = E M.empty Nothing

bind :: String -> Object -> Env -> Env
bind ident val (E store outer) = E (M.insert ident val store) outer

get :: String -> Env -> Maybe Object
get ident (E store outer) = M.lookup ident store <|> (outer >>= get ident)

enclosed :: Env -> Env
enclosed env = E M.empty (Just env)

chain :: Env -> Env -> Env
chain (E store Nothing) outer = E store (Just outer)
chain (E store (Just outer)) outer' = E store (Just (chain outer outer'))

data BuiltIn =
    BuiltInLen
  | BuiltInPuts
  | BuiltInFirst
  | BuiltInLast
  | BuiltInRest
  | BuiltInPush
  deriving (Show, Eq)

data Object =
    ObjNull
  | ObjInt Int
  | ObjBool Bool
  | ObjString String
  | ObjReturn Object
  | ObjBuiltIn BuiltIn
  | ObjArray [Object]
  | ObjFn [String] [Ast.Stmt] Env
  deriving (Eq, Show)

objType :: Object -> String
objType ObjNull = "NULL"
objType (ObjInt _) = "INTEGER"
objType (ObjBool _) = "BOOLEAN"
objType (ObjString _) = "STRING"
objType (ObjReturn _) = "RETURN"
objType (ObjBuiltIn _) = "BUILTIN"
objType (ObjArray _) = "ARRAY"
objType (ObjFn {}) = "FN"
