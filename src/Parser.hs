module Parser (parseProgram) where

import Control.Applicative
import Data.Char (isDigit)
import Control.Arrow (first)

import Token
import Ast
import Lexer

type Error = String
type PrefixParseFn = [Token] -> (Maybe Expr, [Token])
type InfixParseFn = [Token] -> Expr -> (Maybe Expr, [Token])

parseProgram :: String -> Either Error Program
parseProgram src = parseStmts (tokens src) [] >>= f where
  f (_, unconsumed@(t:_)) = Left ("Unconsumed input: " ++ show unconsumed)
  f (stmts, _) = Right (Program stmts)

parseStmts :: [Token] -> [Stmt] -> Either Error ([Stmt], [Token])
parseStmts [] stmts = Right (reverse stmts, [])
parseStmts ts stmts = parseStmt ts >>= f where
  f (Just stmt, remaining) = parseStmts remaining (stmt:stmts)
  f (Nothing, remaining) = Right (reverse stmts, remaining)

parseStmt :: [Token] -> Either Error (Maybe Stmt, [Token])
parseStmt [] = Right (Nothing, [])
parseStmt ((T Let _):ts) = fmap (first Just) (parseLetStmt ts)
parseStmt ((T Return _):ts) = Right (Just (ReturnStmt $ IntLiteral 0), skipExpr ts)
parseStmt ts = parseExprStmt ts

parseExprStmt :: [Token] -> Either Error (Maybe Stmt, [Token])
parseExprStmt ts = fmap toStmt (parseExpr Lowest ts) where
  toStmt (Nothing, ts) = (Nothing, ts)
  toStmt (Just expr, (T SemiColon _):ts) = (Just (ExprStmt expr), ts)
  toStmt (Just expr, ts) = (Just (ExprStmt expr), ts)

parseExpr :: Prec -> [Token] -> Either Error (Maybe Expr, [Token])
parseExpr prec (t:ts) = case prefixParser t of
  Nothing -> Right (Nothing, ts)
  Just parser -> Right (parser ts)

parseLetStmt :: [Token] -> Either Error (Stmt, [Token])
parseLetStmt (ident@(T Ident _):(T Assign _):rest) =
  Right (LetStmt ident $ IntLiteral 0, skipExpr rest)
parseLetStmt ((T kind _):_) = Left ("Expected T.Ident, found " ++ show kind)
parseLetStmt [] = Left "Expected T.Ident, found EOF"

prefixParser :: Token -> Maybe PrefixParseFn
prefixParser (T Ident name) = Just (\ts -> (Just $ Identifier name, ts))
prefixParser (T Int lxm) = Just (\ts -> (Just $ IntLiteral (read lxm), ts))
prefixParser t = error $ "no prefix fn found for " ++ show t

infixParser :: Token -> Maybe InfixParseFn
infixParser = undefined

data Prec =
    Lowest
  | Equals
  | LessGreater
  | Sum
  | Product
  | Prefix
  | Call
  deriving (Show, Eq, Ord)

-- temp, till we parse expressions
skipExpr :: [Token] -> [Token]
skipExpr [] = []
skipExpr ((T SemiColon _):rest) = rest
skipExpr (_:rest) = skipExpr rest

--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

newtype XParser i o = XParser (i -> Maybe (o, i))

parse :: XParser i o -> i -> Maybe (o, i)
parse (XParser f) = f

failure :: XParser i o
failure = XParser (const Nothing)

instance Functor (XParser i) where
  -- fmap :: (a -> b) -> XParser i a -> XParser i b
  fmap f p = XParser (\inp -> case parse p inp of
    Nothing -> Nothing
    Just(x, rest) -> Just(f x, rest))

instance Applicative (XParser i) where
  -- pure :: a -> XParser i a
  pure x = XParser (\inp -> Just(x, inp))
  -- (<*>) :: XParser i (a -> b) -> XParser i a -> XParser i b
  pf <*> px = XParser (\inp -> case parse pf inp of
    Nothing -> Nothing
    Just(f, rest) -> parse (fmap f px) rest)

instance Monad (XParser i) where
  -- (>>=) :: XParser i a -> (a -> XParser i b) -> XParser i b
  px >>= f = XParser (\inp -> case parse px inp of
    Nothing -> Nothing
    Just(x, rest) -> parse (f x) rest)

instance Alternative (XParser i) where
  -- empty :: XParser i o
  empty = failure
  -- (<|>) :: XParser i o -> XParser i o -> XParser i o
  p1 <|> p2 = XParser (\inp -> case parse p1 inp of
    Nothing -> parse p2 inp
    result -> result)

anyChar :: XParser String Char
anyChar = XParser (\inp -> case inp of
  "" -> Nothing
  (x:xs) -> Just(x, xs))

charSatisfies :: (Char -> Bool) -> XParser String Char
charSatisfies predicate = do
  ch <- anyChar
  if predicate ch then return ch else failure

digit :: XParser String Char
digit = charSatisfies isDigit

char :: Char -> XParser String Char
char x = charSatisfies (== x)

single :: Char -> TokenType -> XParser String Token
single ch t = do
  lexeme <- char ch
  return (token t [lexeme])
