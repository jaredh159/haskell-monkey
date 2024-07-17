module Parser (Parser(..), parse, failure) where

import Control.Applicative
import Data.Char (isDigit)

import Token

newtype Parser i o = Parser (i -> Maybe (o, i))

parse :: Parser i o -> i -> Maybe (o, i)
parse (Parser f) = f

failure :: Parser i o
failure = Parser (const Nothing)

instance Functor (Parser i) where
  -- fmap :: (a -> b) -> Parser i a -> Parser i b
  fmap f p = Parser (\inp -> case parse p inp of
    Nothing -> Nothing
    Just(x, rest) -> Just(f x, rest))

instance Applicative (Parser i) where
  -- pure :: a -> Parser i a
  pure x = Parser (\inp -> Just(x, inp))
  -- (<*>) :: Parser i (a -> b) -> Parser i a -> Parser i b
  pf <*> px = Parser (\inp -> case parse pf inp of
    Nothing -> Nothing
    Just(f, rest) -> parse (fmap f px) rest)

instance Monad (Parser i) where
  -- (>>=) :: Parser i a -> (a -> Parser i b) -> Parser i b
  px >>= f = Parser (\inp -> case parse px inp of
    Nothing -> Nothing
    Just(x, rest) -> parse (f x) rest)

instance Alternative (Parser i) where
  -- empty :: Parser i o
  empty = failure
  -- (<|>) :: Parser i o -> Parser i o -> Parser i o
  p1 <|> p2 = Parser (\inp -> case parse p1 inp of
    Nothing -> parse p2 inp
    result -> result)

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
