module Token
  ( Token(..)
  , token
  , TokenType(..)
  ) where

data Token = Tok TokenType String
  deriving (Show, Eq)

token :: TokenType -> String -> Token
token = Tok

data TokenType =
    Illegal
  | Eof
  | Ident
  | Int
  | Assign
  | Plus
  | Comma
  | SemiColon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Function
  | Let
  | Minus
  | Bang
  | MString
  | Asterisk
  | Slash
  | Lt
  | Gt
  | MTrue
  | MFalse
  | If
  | Else
  | Return
  | Eq
  | NotEq
  deriving (Show, Eq, Ord)
