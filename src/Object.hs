module Object (
    Object(..)
  , objType
  ) where

data Object =
    ObjNull
  | ObjInt Int
  | ObjBool Bool
  | ObjReturn Object
  deriving (Eq, Show)

objType :: Object -> String
objType ObjNull = "NULL"
objType (ObjInt _) = "INTEGER"
objType (ObjBool _) = "BOOLEAN"
objType (ObjReturn _) = "RETURN"

