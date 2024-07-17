module Lexer (nextToken, tokens) where

import Control.Monad.State
import Control.Applicative
import Debug.Trace (trace)

import Parser
import Token
import Data.Char (isAlpha, isDigit)

nextToken :: String -> Maybe (Token, String)
nextToken "" = Nothing
nextToken (ch:rest)
  | ch == '+' = Just (token Plus [ch], rest)
  | ch == '-' = Just (token Minus [ch], rest)
  | ch == '/' = Just (token Slash [ch], rest)
  | ch == '*' = Just (token Asterisk [ch], rest)
  | ch == '<' = Just (token Lt [ch], rest)
  | ch == '>' = Just (token Gt [ch], rest)
  | ch == '(' = Just (token LParen [ch], rest)
  | ch == ')' = Just (token RParen [ch], rest)
  | ch == '{' = Just (token LBrace [ch], rest)
  | ch == '}' = Just (token RBrace [ch], rest)
  | ch == ',' = Just (token Comma [ch], rest)
  | ch == ';' = Just (token SemiColon [ch], rest)
  -- | ch == '=' = Just (token Assign [ch], rest)
  | ch == '=' = case rest of
    ('=':rest') -> Just (token Eq "==", rest')
    _ -> Just (token Assign [ch], rest)
  -- | ch == '=' = Just (token Assign [ch], rest)
  | ch == '!' = case rest of
    ('=':rest') -> Just (token NotEq "!=", rest')
    _ -> Just (token Bang [ch], rest)
  | isLetter ch = Just $ ident $ word [ch] rest
  | isDigit ch = Just $ number [ch] rest
  | isWhitespace ch = nextToken rest
  | otherwise = error ("Unhandled char: `" ++ [ch] ++ "`")

word :: String -> String -> (String, String)
word acc "" = (reverse acc, "")
word acc (ch:rest) = if isLetter ch
  then word (ch:acc) rest
  else (reverse acc, ch:rest)

ident :: (String, String) -> (Token, String)
ident ("let", src) = (token Let "let", src)
ident ("fn", src) = (token Function "fn", src)
ident ("if", src) = (token If "if", src)
ident ("else", src) = (token Else "else", src)
ident ("true", src) = (token MTrue "true", src)
ident ("false", src) = (token MFalse "false", src)
ident ("return", src) = (token Return "return", src)
ident (lexeme, src) = (token Ident lexeme, src)

number :: String -> String -> (Token, String)
number acc "" = (token Int $ reverse acc, "")
number acc (ch:rest) = if isDigit ch
  then number (ch:acc) rest
  else (token Int $ reverse acc, ch:rest)

isLetter :: Char -> Bool
isLetter c = isAlpha c || c == '_'

isWhitespace :: Char -> Bool
isWhitespace ch = ch `elem` [' ', '\t', '\n', '\r']

tokens :: String -> [Token]
tokens input = gather input []

gather :: String -> [Token] -> [Token]
gather "" acc = reverse acc
gather inp acc = case nextToken inp of
  Just (tok, rest) -> gather rest (tok:acc)
  Nothing -> error ("Unconsumed input: " ++ inp)

