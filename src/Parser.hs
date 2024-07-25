module Parser (parseProgram) where

import Control.Applicative
import Data.Char (isDigit)
import Control.Arrow (first)

import qualified Token as T
import qualified Ast
import qualified Lexer
import Debug.Trace (trace)

type Error = String
type ParseResult a = Either Error (a, [T.Token])
type PrefixParseFn = [T.Token] -> ParseResult (Maybe Ast.Expr)
type InfixParseFn = [T.Token] -> Ast.Expr -> ParseResult (Maybe Ast.Expr)

parseProgram :: String -> Either Error Ast.Program
parseProgram src = parseStmts (Lexer.tokens src) [] >>= f where
  f (_, unconsumed@(t:_)) = Left ("Unconsumed input: " ++ show unconsumed)
  f (stmts, _) = Right (Ast.Program stmts)

parseStmts :: [T.Token] -> [Ast.Stmt] -> ParseResult [Ast.Stmt]
parseStmts [] stmts = Right (reverse stmts, [])
parseStmts ts stmts = parseStmt ts >>= f where
  f (Just stmt, remaining) = parseStmts remaining (stmt:stmts)
  f (Nothing, remaining) = Right (reverse stmts, remaining)

parseStmt :: [T.Token] -> ParseResult (Maybe Ast.Stmt)
parseStmt [] = Right (Nothing, [])
parseStmt ((T.Tok T.Let _):ts) = fmap (first Just) (parseLetStmt ts)
parseStmt ((T.Tok T.Return _):ts) =
  Right (Just (Ast.ReturnStmt $ Ast.IntLiteral 0), skipExpr ts)
parseStmt ts = parseExprStmt ts

parseExprStmt :: [T.Token] -> ParseResult (Maybe Ast.Stmt)
parseExprStmt ts = fmap toStmt (parseExpr Lowest ts) where
  toStmt (Nothing, ts) = (Nothing, ts)
  toStmt (Just expr, (T.Tok T.SemiColon _):ts) = (Just (Ast.ExprStmt expr), ts)
  toStmt (Just expr, ts) = (Just (Ast.ExprStmt expr), ts)

parseExpr :: Prec -> [T.Token] -> ParseResult (Maybe Ast.Expr)
parseExpr prec (t:ts) = case prefixParser t of
  Nothing -> Left ("No prefix parser found for " ++ show t)
  Just parsePrefix -> parsePrefix ts >>= descend prec

descend :: Prec -> (Maybe Ast.Expr, [T.Token]) -> ParseResult (Maybe Ast.Expr)
descend curprec (Just expr, ts@(peek:_))
  | curprec < precedence peek = case infixParser peek of
    Nothing -> Right (Just expr, ts)
    Just parseInfix -> parseInfix ts expr >>= descend curprec
  | otherwise = Right (Just expr, ts)
descend _ result = Right result

parseLetStmt :: [T.Token] -> ParseResult Ast.Stmt
parseLetStmt (ident@(T.Tok T.Ident _):(T.Tok T.Assign _):rest) =
  Right (Ast.LetStmt ident $ Ast.IntLiteral 0, skipExpr rest)
parseLetStmt ((T.Tok kind _):_) = Left ("Expected T.Ident, found " ++ show kind)
parseLetStmt [] = Left "Expected T.Ident, found EOF"

prefixParser :: T.Token -> Maybe PrefixParseFn
prefixParser (T.Tok T.Ident name) = Just (\ts -> Right (Just $ Ast.Ident name, ts))
prefixParser (T.Tok T.Int int) = Just (\ts -> Right (Just $ Ast.IntLiteral (read int), ts))
prefixParser (T.Tok T.Bang _) = Just (parsePrefixExpr Ast.PrefixBang)
prefixParser (T.Tok T.Minus _) = Just (parsePrefixExpr Ast.PrefixMinus)
prefixParser _ = Nothing

infixParser :: T.Token -> Maybe InfixParseFn
infixParser (T.Tok T.Plus _) = Just (parseInfixExpr Ast.InfixPlus)
infixParser (T.Tok T.Minus _) = Just (parseInfixExpr Ast.InfixMinus)
infixParser (T.Tok T.Slash _) = Just (parseInfixExpr Ast.InfixSlash)
infixParser (T.Tok T.Lt _) = Just (parseInfixExpr Ast.InfixLt)
infixParser (T.Tok T.Gt _) = Just (parseInfixExpr Ast.InfixGt)
infixParser (T.Tok T.Eq _) = Just (parseInfixExpr Ast.InfixEq)
infixParser (T.Tok T.NotEq _) = Just (parseInfixExpr Ast.InfixNotEq)
infixParser (T.Tok T.Asterisk _) = Just (parseInfixExpr Ast.InfixAsterisk)
infixParser _ = Nothing

parseInfixExpr :: Ast.InfixOp -> [T.Token] -> Ast.Expr -> ParseResult (Maybe Ast.Expr)
parseInfixExpr op (t:ts) lhs = do
  result <- parseExpr (precedence t) ts
  return $ first (fmap $ Ast.Infix lhs op) result

parsePrefixExpr :: Ast.PrefixOp -> [T.Token] -> ParseResult (Maybe Ast.Expr)
parsePrefixExpr op ts = do
  result <- parseExpr Prefix ts
  return $ first (fmap $ Ast.Prefix op) result

precedence :: T.Token -> Prec
precedence (T.Tok T.Eq _) = Equals
precedence (T.Tok T.NotEq _) = Equals
precedence (T.Tok T.Lt _) = LessGreater
precedence (T.Tok T.Gt _) = LessGreater
precedence (T.Tok T.Plus _) = Sum
precedence (T.Tok T.Minus _) = Sum
precedence (T.Tok T.Slash _) = Product
precedence (T.Tok T.Asterisk _) = Product
precedence _ = Lowest

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
skipExpr :: [T.Token] -> [T.Token]
skipExpr [] = []
skipExpr ((T.Tok T.SemiColon _):rest) = rest
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

single :: Char -> T.TokenType -> XParser String T.Token
single ch t = do
  lexeme <- char ch
  return (T.token t [lexeme])
