module Object (
    Object(..)
  ) where

data Object =
    ObjNull
  | ObjInt Int
  | ObjBool Bool
  deriving (Eq, Show)

