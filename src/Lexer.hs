module Lexer
( Lexer
, lexer
, nextToken
) where

import Control.Monad.State
import Token

data Lexer = Lexer
  { source :: String
  , idx :: Int
  } deriving (Show)

lexer :: String -> Lexer
lexer src = Lexer src 0

nextToken :: State Lexer Token
-- nextToken = get >>= \lexer -> return $ token Eof ""
nextToken = state $ \lexer -> (token Eof "", lexer)

add1 :: Int -> Int
add1 x = x + 2

