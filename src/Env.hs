module Env
  ( Env
  , empty
  , enclosed
  , bind
  , get
  , BuiltIn(..)
  , Object(..)
  , objType
  , printObj
  , chain
  ) where

import qualified Data.Map as M
import qualified Ast
import Control.Applicative ((<|>))
import Data.List (intercalate)

data Env = E (M.Map String Object) (Maybe Env)
  deriving (Eq, Show, Ord)

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
  deriving (Show, Eq, Ord)

data Object =
    ObjNull
  | ObjInt Int
  | ObjBool Bool
  | ObjString String
  | ObjReturn Object
  | ObjBuiltIn BuiltIn
  | ObjArray [Object]
  | ObjHash (M.Map Object Object)
  | ObjFn [String] [Ast.Stmt] Env
  deriving (Eq, Show, Ord)

objType :: Object -> String
objType ObjNull = "NULL"
objType (ObjInt _) = "INTEGER"
objType (ObjBool _) = "BOOLEAN"
objType (ObjString _) = "STRING"
objType (ObjReturn _) = "RETURN"
objType (ObjBuiltIn _) = "BUILTIN"
objType (ObjArray _) = "ARRAY"
objType (ObjFn {}) = "FN"
objType (ObjHash {}) = "HASH"

printObj :: Object -> String
printObj (ObjInt i) = show i
printObj (ObjBool True) = "true"
printObj (ObjBool False) = "false"
printObj ObjNull = "null"
printObj (ObjFn {}) = "<fn>"
printObj (ObjString string) = "\"" ++ string ++ "\""
printObj (ObjBuiltIn _) = "<builtin fn>"
printObj (ObjArray objs) = "[" ++ intercalate ", " (map printObj objs) ++ "]"
printObj (ObjReturn _) = error "unreachable"
printObj (ObjHash hashmap) =
  let entries = map (\(key, val) -> printObj key ++ ": " ++ printObj val) (M.toList hashmap) in
  "{" ++ intercalate ", " entries ++ "}"
