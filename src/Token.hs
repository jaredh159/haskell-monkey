module Token (Token, TokenType(..)) where

data Token = Token
  { typ :: TokenType
  , literal :: String
  } deriving (Show, Eq)

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
  deriving (Show, Eq, Ord)
