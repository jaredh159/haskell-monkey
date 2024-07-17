module Token (Token, token, TokenType(..)) where

data Token = Token
  { typ :: TokenType
  , literal :: String
  } deriving (Show, Eq)

token :: TokenType -> String -> Token
token t l = Token {typ=t, literal=l}

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
