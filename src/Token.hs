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
  | Colon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
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
