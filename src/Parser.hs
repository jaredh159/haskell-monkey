module Parser
  ( parseProgram
  ) where

import Control.Arrow (first)

import qualified Token as T
import qualified Ast
import qualified Lexer

type Error = String
type Result a = (a, [T.Token])
type ParseResult a = Either Error (Result a)
type PrefixParseFn = [T.Token] -> ParseResult (Maybe Ast.Expr)
type InfixParseFn = [T.Token] -> Ast.Expr -> ParseResult (Maybe Ast.Expr)

parseProgram :: String -> Either Error Ast.Program
parseProgram src = parseStmts (Lexer.tokens src) [] >>= f where
  f (_, unconsumed@(_:_)) = Left ("Unconsumed input: " ++ show unconsumed)
  f (stmts, _) = Right stmts

-- statements

parseStmts :: [T.Token] -> [Ast.Stmt] -> ParseResult [Ast.Stmt]
parseStmts [] stmts = Right (reverse stmts, [])
parseStmts ts stmts = parseStmt ts >>= f where
  f (Just stmt, remaining) = parseStmts remaining (stmt:stmts)
  f (Nothing, remaining) = Right (reverse stmts, remaining)

parseStmt :: [T.Token] -> ParseResult (Maybe Ast.Stmt)
parseStmt [] = Right (Nothing, [])
parseStmt ((T.Tok T.Let _):ts) = parseLetStmt ts
parseStmt ((T.Tok T.Return _):ts) =
  fmap (wrap Ast.ReturnStmt) (parseExpr Lowest ts)
parseStmt ts = parseExprStmt ts

parseLetStmt :: [T.Token] -> ParseResult (Maybe Ast.Stmt)
parseLetStmt ((T.Tok T.Ident ident):(T.Tok T.Assign _):ts) =
  fmap (wrap $ Ast.LetStmt ident) (parseExpr Lowest ts)
parseLetStmt ((T.Tok kind _):_) = Left ("Expected T.Ident, found " ++ show kind)
parseLetStmt [] = Left "Expected T.Ident, found EOF"

parseExprStmt :: [T.Token] -> ParseResult (Maybe Ast.Stmt)
parseExprStmt ts = fmap (wrap Ast.ExprStmt) (parseExpr Lowest ts)

wrap :: (Ast.Expr -> Ast.Stmt) -> Result (Maybe Ast.Expr) -> Result (Maybe Ast.Stmt)
wrap _ (Nothing, ts) = (Nothing, ts)
wrap f (Just expr, (T.Tok T.SemiColon _):ts) = (Just (f expr), ts)
wrap f (Just expr, ts) = (Just (f expr), ts)

parseBlockStmt :: [Ast.Stmt] -> [T.Token] -> ParseResult [Ast.Stmt]
parseBlockStmt stmts [] = Right (stmts, [])
parseBlockStmt stmts ((T.Tok T.RBrace _):ts) = Right (stmts, ts)
parseBlockStmt stmts ts = do
  result <- parseStmt ts
  case result of
    (Just stmt, ts') -> parseBlockStmt (stmts ++ [stmt]) ts'
    (Nothing, ts') -> parseBlockStmt stmts ts'

-- expressions

parseExpr :: Prec -> [T.Token] -> ParseResult (Maybe Ast.Expr)
parseExpr prec (t:ts) = case prefixParser t of
  Nothing -> Left ("No prefix parser found for " ++ show t)
  Just parsePrefix -> parsePrefix ts >>= descend prec
parseExpr _ [] = Left "Expected expression, got EOF"

descend :: Prec -> (Maybe Ast.Expr, [T.Token]) -> ParseResult (Maybe Ast.Expr)
descend curprec (Just expr, ts@(peek:_))
  | curprec < precedence peek = case infixParser peek of
    Nothing -> Right (Just expr, ts)
    Just parseInfix -> parseInfix ts expr >>= descend curprec
  | otherwise = Right (Just expr, ts)
descend _ result = Right result

-- prefix expressions

prefixParser :: T.Token -> Maybe PrefixParseFn
prefixParser (T.Tok T.Ident name) = Just (\ts -> Right (Just $ Ast.Ident name, ts))
prefixParser (T.Tok T.Int int) = Just (\ts -> Right (Just $ Ast.IntLit (read int), ts))
prefixParser (T.Tok T.MTrue _) = Just (\ts -> Right (Just $ Ast.BoolLit True, ts))
prefixParser (T.Tok T.MFalse _) = Just (\ts -> Right (Just $ Ast.BoolLit False, ts))
prefixParser (T.Tok T.MString s) = Just (\ts -> Right (Just $ Ast.StringLit s, ts))
prefixParser (T.Tok T.Bang _) = Just (parsePrefixExpr Ast.PrefixBang)
prefixParser (T.Tok T.Minus _) = Just (parsePrefixExpr Ast.PrefixMinus)
prefixParser (T.Tok T.LParen _) = Just parseGroupedExpr
prefixParser (T.Tok T.If _) = Just parseIfExpr
prefixParser (T.Tok T.Function _) = Just parseFunction
prefixParser (T.Tok T.LBracket _) = Just parseArrayLitExpr
prefixParser _ = Nothing

parsePrefixExpr :: Ast.PrefixOp -> [T.Token] -> ParseResult (Maybe Ast.Expr)
parsePrefixExpr op ts = do
  result <- parseExpr Prefix ts
  pure $ first (fmap $ Ast.Prefix op) result

parseFunction :: PrefixParseFn
parseFunction ((T.Tok T.LParen _):ts) = do
  (params, ts') <- parseFnParams [] ts
  case ts' of
    (T.Tok T.LBrace _):ts'' -> do
      (body, ts''') <- parseBlockStmt [] ts''
      Right (Just (Ast.FnLit params body), ts''')
    (t:_) -> Left $ "Expected Tok.LBrace, got: " ++ show t
    [] -> Left "Expected Tok.LBrace, got EOF"
parseFunction ts = error $ "??? " ++ show ts

parseFnParams :: [String] -> [T.Token] -> ParseResult [String]
parseFnParams params ((T.Tok T.RParen _):ts) = Right (params, ts)
parseFnParams params ((T.Tok T.Comma _):(T.Tok T.Ident ident):ts) = parseFnParams (params ++ [ident]) ts
parseFnParams params ((T.Tok T.Ident ident):ts) = parseFnParams (params ++ [ident]) ts
parseFnParams _ (t:_) = Left $ "Expected Tok.RParen or Tok.Ident, got: " ++ show t
parseFnParams _ [] = Left "Expected Tok.RParen or Tok.Ident, got EOF"

parseIfExpr :: PrefixParseFn
parseIfExpr ((T.Tok T.LParen _):ts) = do
  condResult <- parseExpr Lowest ts
  case condResult of
    (Just cond, (T.Tok T.RParen _):(T.Tok T.LBrace _):ts') -> do
      (conseq, ts'') <- parseBlockStmt [] ts'
      case ts'' of
        ((T.Tok T.Else _):(T.Tok T.LBrace _):ts''') -> do
          (alt, ts'''') <- parseBlockStmt [] ts'''
          Right (Just (Ast.If cond conseq (Just alt)), ts'''')
        _ -> Right (Just (Ast.If cond conseq Nothing), ts'')
    (Just _, t:_) -> error $ "Expected Tok.RParen, got: " ++ show t
    (Just _, []) -> error "Expected Tok.RParen, got EOF"
    (Nothing, _) -> error "Expected if expression condition"
parseIfExpr (t:_) = error $ "Expected Tok.LParen, got: " ++ show t
parseIfExpr [] = error "Expected Tok.LParen, got EOF"

parseGroupedExpr :: PrefixParseFn
parseGroupedExpr ts = do
  result <- parseExpr Lowest ts
  case result of
    (Nothing, ts') -> Right (Nothing, ts')
    (Just expr, (T.Tok T.RParen _):ts') -> Right (Just expr, ts')
    (Just _, []) -> Left "Expected Tok.RParen, found EOF"
    (Just _, t:_) -> Left $ "Expected Tok.RParen, found: " ++ show t

parseArrayLitExpr :: PrefixParseFn
parseArrayLitExpr ts = do
  (exprs, ts') <- parseExprList T.RBracket [] ts
  Right (Just $ Ast.ArrayLit exprs, ts')

-- infix expressons

parseInfixExpr :: Ast.InfixOp -> InfixParseFn
parseInfixExpr op (t:ts) lhs = do
  result <- parseExpr (precedence t) ts
  pure $ first (fmap $ Ast.Infix lhs op) result
parseInfixExpr _ [] _ = Left "Expected InfixOp token, got EOF"

parseCallExpr :: InfixParseFn
parseCallExpr ((T.Tok T.LParen _):ts) fn = do
  (args, ts') <- parseExprList T.RParen [] ts
  Right (Just (Ast.Call fn args), ts')
parseCallExpr (t:_) _ = Left $ "Expected Tok.LParen, got:" ++ show t
parseCallExpr [] _ = Left "Expected Tok.LParen, got EOF"

parseExprList :: T.TokenType -> [Ast.Expr] -> [T.Token] -> ParseResult [Ast.Expr]
parseExprList end args ts = case (end, ts) of
  (T.RParen, (T.Tok T.RParen _):rest) -> Right (args, rest)
  (T.RBracket, (T.Tok T.RBracket _):rest) -> Right (args, rest)
  (T.RParen, (T.Tok T.Comma _):(T.Tok T.RBracket _):_) ->
    Left "Unexpected trailing comma in expression list"
  (T.RParen, (T.Tok T.Comma _):(T.Tok T.RParen _):_) ->
    Left "Unexpected trailing comma in expression list"
  _ -> case ts of
    ((T.Tok T.Comma _):rest) -> parseExprList end args rest
    ts'@(_:_) -> do
      argResult <- parseExpr Lowest ts'
      case argResult of
        (Just arg, ts'') -> parseExprList end (args ++ [arg]) ts''
        (Nothing, ts'') -> parseExprList end args ts''
    [] -> Left "Unexpected EOF in expression list"

parseIndexExpr :: InfixParseFn
parseIndexExpr (_:ts) lhs = do
  result <- parseExpr Lowest ts
  case result of
    (Just idx, (T.Tok T.RBracket _):ts') -> Right (Just (Ast.Index lhs idx), ts')
    _ -> Left "Unexpected failure parsing index expr"
parseIndexExpr [] _ = undefined -- unreachable

infixParser :: T.Token -> Maybe InfixParseFn
infixParser (T.Tok T.Plus _) = Just (parseInfixExpr Ast.InfixPlus)
infixParser (T.Tok T.Minus _) = Just (parseInfixExpr Ast.InfixMinus)
infixParser (T.Tok T.Slash _) = Just (parseInfixExpr Ast.InfixSlash)
infixParser (T.Tok T.Lt _) = Just (parseInfixExpr Ast.InfixLt)
infixParser (T.Tok T.Gt _) = Just (parseInfixExpr Ast.InfixGt)
infixParser (T.Tok T.Eq _) = Just (parseInfixExpr Ast.InfixEq)
infixParser (T.Tok T.NotEq _) = Just (parseInfixExpr Ast.InfixNotEq)
infixParser (T.Tok T.Asterisk _) = Just (parseInfixExpr Ast.InfixAsterisk)
infixParser (T.Tok T.LParen _) = Just parseCallExpr
infixParser (T.Tok T.LBracket _) = Just parseIndexExpr
infixParser _ = Nothing

-- precedence

precedence :: T.Token -> Prec
precedence (T.Tok T.Eq _) = Equals
precedence (T.Tok T.NotEq _) = Equals
precedence (T.Tok T.Lt _) = LessGreater
precedence (T.Tok T.Gt _) = LessGreater
precedence (T.Tok T.Plus _) = Sum
precedence (T.Tok T.Minus _) = Sum
precedence (T.Tok T.Slash _) = Product
precedence (T.Tok T.Asterisk _) = Product
precedence (T.Tok T.LParen _) = Call
precedence (T.Tok T.LBracket _) = Index
precedence _ = Lowest

data Prec =
    Lowest
  | Equals
  | LessGreater
  | Sum
  | Product
  | Prefix
  | Call
  | Index
  deriving (Show, Eq, Ord)

