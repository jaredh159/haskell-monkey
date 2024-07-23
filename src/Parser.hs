module Parser (parseProgram) where

import Control.Applicative
import Data.Char (isDigit)
import Control.Arrow (first)

import qualified Token as T
import qualified Ast
import qualified Lexer

type Error = String
type PrefixParseFn = [T.Token] -> Either Error (Maybe Ast.Expr, [T.Token])
type InfixParseFn = [T.Token] -> Ast.Expr -> Either Error (Maybe Ast.Expr, [T.Token])

parseProgram :: String -> Either Error Ast.Program
parseProgram src = parseStmts (Lexer.tokens src) [] >>= f where
  f (_, unconsumed@(t:_)) = Left ("Unconsumed input: " ++ show unconsumed)
  f (stmts, _) = Right (Ast.Program stmts)

parseStmts :: [T.Token] -> [Ast.Stmt] -> Either Error ([Ast.Stmt], [T.Token])
parseStmts [] stmts = Right (reverse stmts, [])
parseStmts ts stmts = parseStmt ts >>= f where
  f (Just stmt, remaining) = parseStmts remaining (stmt:stmts)
  f (Nothing, remaining) = Right (reverse stmts, remaining)

parseStmt :: [T.Token] -> Either Error (Maybe Ast.Stmt, [T.Token])
parseStmt [] = Right (Nothing, [])
parseStmt ((T.T T.Let _):ts) = fmap (first Just) (parseLetStmt ts)
parseStmt ((T.T T.Return _):ts) =
  Right (Just (Ast.ReturnStmt $ Ast.IntLiteral 0), skipExpr ts)
parseStmt ts = parseExprStmt ts

parseExprStmt :: [T.Token] -> Either Error (Maybe Ast.Stmt, [T.Token])
parseExprStmt ts = fmap toStmt (parseExpr Lowest ts) where
  toStmt (Nothing, ts) = (Nothing, ts)
  toStmt (Just expr, (T.T T.SemiColon _):ts) = (Just (Ast.ExprStmt expr), ts)
  toStmt (Just expr, ts) = (Just (Ast.ExprStmt expr), ts)

parseExpr :: Prec -> [T.Token] -> Either Error (Maybe Ast.Expr, [T.Token])
parseExpr prec (t:ts) = case prefixParser t of
  Nothing -> Right (Nothing, ts)
  Just parse -> parse ts

parseLetStmt :: [T.Token] -> Either Error (Ast.Stmt, [T.Token])
parseLetStmt (ident@(T.T T.Ident _):(T.T T.Assign _):rest) =
  Right (Ast.LetStmt ident $ Ast.IntLiteral 0, skipExpr rest)
parseLetStmt ((T.T kind _):_) = Left ("Expected T.Ident, found " ++ show kind)
parseLetStmt [] = Left "Expected T.Ident, found EOF"

prefixParser :: T.Token -> Maybe PrefixParseFn
prefixParser (T.T T.Ident name) =
  Just (\ts -> Right (Just $ Ast.Ident name, ts))
prefixParser (T.T T.Int int) =
  Just (\ts -> Right (Just $ Ast.IntLiteral (read int), ts))
prefixParser (T.T T.Bang _) = Just $ parsePrefixExpr Ast.PrefixBang
prefixParser (T.T T.Minus _) = Just $ parsePrefixExpr Ast.PrefixMinus
prefixParser t = error $ "no prefix fn found for " ++ show t

parsePrefixExpr :: Ast.PrefixOp -> [T.Token] -> Either Error (Maybe Ast.Expr, [T.Token])
parsePrefixExpr op ts = do
  expr <- parseExpr Prefix ts
  return $ first (fmap $ Ast.Prefix op) expr

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
skipExpr ((T.T T.SemiColon _):rest) = rest
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
