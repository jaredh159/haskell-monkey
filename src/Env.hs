module Env
  ( Env
  , empty
  , bind
  , get
  ) where

import qualified Data.Map as M

import Object

data Env = E (M.Map String Object) (Maybe Env)

empty :: Env
empty = E M.empty Nothing

bind :: String -> Object -> Env -> Env
bind ident val (E store outer) = E (M.insert ident val store) outer

get :: String -> Env -> Maybe Object
get ident (E store _) = M.lookup ident store
