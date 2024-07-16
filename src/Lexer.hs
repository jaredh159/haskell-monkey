module Lexer (nextToken) where

import Control.Monad.State
import Control.Applicative
import Debug.Trace (trace)

import Parser
import Token
import Data.Char (isDigit)

anyChar :: Parser String Char
anyChar = Parser (\inp -> case inp of
  "" -> Nothing
  (x:xs) -> Just(x, xs))

charSatisfies :: (Char -> Bool) -> Parser String Char
charSatisfies predicate = do
  ch <- anyChar
  if predicate ch then return ch else failure

digit :: Parser String Char
digit = charSatisfies isDigit

char :: Char -> Parser String Char
char x = charSatisfies (== x)

single :: Char -> TokenType -> Parser String Token
single ch t = do
  lexeme <- char ch
  return (token t [lexeme])

-- data Lexer = Lexer
--   { source :: String
--   , idx :: Int
--   } deriving (Show)

-- lexer :: String -> Lexer
-- lexer src = Lexer src 0

nextToken :: State String (Maybe Token)
-- nextToken = get >>= \lexer -> return $ token Eof ""
nextToken = state $ \src -> parse (single '=' Assign) src

add1 :: Int -> Int
add1 x = x + 2

