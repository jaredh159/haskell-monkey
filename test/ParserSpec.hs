module ParserSpec (spec) where

import Test.Hspec
import Parser
import qualified Ast
import Token
import Data.Either (fromRight)

spec :: Spec
spec = do
  it "should parse correct ident names" $ do
    let input = "let x = 5; let y = 10; let foobar = 838383;"
    map letStmtName (stmts input) `shouldBe` ["x", "y", "foobar"]

  it "should recognize return statments" $ do
    let input = "return 5; return 10; return 993322;"
    all isReturnStmt (stmts input) `shouldBe` True

  it "should parse identifier expressions" $ do
    let tolit (Ast.ExprStmt ident) = lit ident
    fmap tolit (stmts "foo; bar") `shouldBe` [LitIdent "foo", LitIdent "bar"]

  it "should parse boolean literal expressions" $ do
    let input = "true; false;"
    let boolVal (Ast.ExprStmt (Ast.BoolLit val)) = val
    fmap boolVal (stmts input) `shouldBe` [True, False]

  it "should parse prefix expressions" $ do
    let pair (Ast.ExprStmt (Ast.Prefix op expr)) = (op, lit expr)
    let cases =
          [ ("!5;", (Ast.PrefixBang, LitInt 5))
          , ("-15;", (Ast.PrefixMinus, LitInt 15))
          , ("!true;", (Ast.PrefixBang, LitBool True))
          , ("!false;", (Ast.PrefixBang, LitBool False))
          ]
    map ((pair . stmt) . fst) cases `shouldBe` map snd cases

  it "should parse infix expressions" $ do
    let triple (Ast.Infix lhs op rhs) = (lit lhs, op, lit rhs)
    let cases =
          [ ("5 + 5", (LitInt 5, Ast.InfixPlus, LitInt 5))
          , ("5 - 5", (LitInt 5, Ast.InfixMinus, LitInt 5))
          , ("5 / 5", (LitInt 5, Ast.InfixSlash, LitInt 5))
          , ("5 > 5", (LitInt 5, Ast.InfixGt, LitInt 5))
          , ("5 < 5", (LitInt 5, Ast.InfixLt, LitInt 5))
          , ("5 == 5", (LitInt 5, Ast.InfixEq, LitInt 5))
          , ("5 != 5", (LitInt 5, Ast.InfixNotEq, LitInt 5))
          , ("true == true", (LitBool True, Ast.InfixEq, LitBool True))
          , ("true != false", (LitBool True, Ast.InfixNotEq, LitBool False))
          ]
    map ((triple . singleExpr) . fst) cases `shouldBe` map snd cases


  it "should handle precedence correctly" $ do
    let cases =
         [ ("-a * b", "((-a) * b)")
         , ("!-a", "(!(-a))")
         , ("true", "true")
         , ("3 > 5 == false", "((3 > 5) == false)")
         , ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")
         , ("(5 + 5) * 2", "((5 + 5) * 2)")
         , ("2 / (5 + 5)", "(2 / (5 + 5))")
         , ("-(5 + 5)", "(-(5 + 5))")
         , ("!(true == true)", "(!(true == true))")
         , ("a + b + c", "((a + b) + c)")
         , ("a + b - c", "((a + b) - c)")
         , ("a * b * c", "((a * b) * c)")
         , ("a * b / c", "((a * b) / c)")
         , ("a + b / c", "(a + (b / c))")
         , ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)")
         , ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")
         , ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))")
         , ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))")
         , ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
         , ("a + add(b * c) + d", "((a + add((b * c))) + d)")
         , ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))")
         ]
    map ((Ast.stringify . program) . fst) cases `shouldBe` map snd cases

  it "should parse if expressions" $ do
    case singleExpr "if (x < y) { x }" of
      (Ast.If cond [cons] Nothing) -> do
        assertInfix cond (LitIdent "x", Ast.InfixLt, LitIdent "y")
        case cons of
          (Ast.ExprStmt (Ast.Ident "x")) -> return ()
          s -> expectationFailure $ "Unexpected cons stmt: " ++ show s
        return ()
      expr -> expectationFailure $ "Unexpected expr: " ++ show expr

  it "should parse if/else expressions" $ do
    case singleExpr "if (x < y) { x } else { y }" of
      (Ast.If cond [cons] (Just [alt])) -> do
        assertInfix cond (LitIdent "x", Ast.InfixLt, LitIdent "y")
        case cons of
          (Ast.ExprStmt (Ast.Ident "x")) -> return ()
          s -> expectationFailure $ "Unexpected cons stmt: " ++ show s
        case alt of
          (Ast.ExprStmt (Ast.Ident "y")) -> return ()
          s -> expectationFailure $ "Unexpected alt stmt: " ++ show s
        return ()
      expr -> expectationFailure $ "Unexpected expr: " ++ show expr

  it "should parse function literals" $ do
    case singleExpr "fn(x, y) { x + y; }" of
      (Ast.FnLit ["x", "y"] [Ast.ExprStmt expr]) -> do
        assertInfix expr (LitIdent "x", Ast.InfixPlus, LitIdent "y")
        return ()
      expr -> expectationFailure $ "Unexpected expr: " ++ show expr
    case singleExpr "fn() {}" of
      (Ast.FnLit [] []) -> do return ()
      expr -> expectationFailure $ "Unexpected expr: " ++ show expr
    case singleExpr "fn(x) {}" of
      (Ast.FnLit ["x"] []) -> do return ()
      expr -> expectationFailure $ "Unexpected expr: " ++ show expr
    case singleExpr "fn(x, y, z, a) {}" of
      (Ast.FnLit ["x", "y", "z", "a"] []) -> do return ()
      expr -> expectationFailure $ "Unexpected expr: " ++ show expr

  it "should parse call expressions" $ do
    case singleExpr "add(1, 2 * 3, 4 + 5)" of
      (Ast.Call (Ast.Ident "add") [Ast.IntLit 1, a2, a3]) -> do
        assertInfix a2 (LitInt 2, Ast.InfixAsterisk, LitInt 3)
        assertInfix a3 (LitInt 4, Ast.InfixPlus, LitInt 5)
        return ()
      expr -> expectationFailure $ "Unexpected expr: " ++ show expr
    case singleExpr "frobnicate()" of
      (Ast.Call (Ast.Ident "frobnicate") []) -> do return ()
      expr -> expectationFailure $ "Unexpected expr: " ++ show expr

-- helpers

assertInfix :: Ast.Expr -> (Lit, Ast.InfixOp, Lit) -> IO ()
assertInfix ifx@(Ast.Infix lhs op rhs) (el, eop, er)
  = if (lit lhs == el) && (op == eop) && (lit rhs == er)
    then return ()
    else expectationFailure $ "Infix expr did not match expectation: " ++ show ifx
assertInfix expr _ = expectationFailure $ "Expected infix, got: " ++ show expr

data Lit = LitInt Int | LitIdent String | LitBool Bool deriving (Show, Eq)

lit :: Ast.Expr -> Lit
lit (Ast.IntLit i) = LitInt i
lit (Ast.Ident i) = LitIdent i
lit (Ast.BoolLit b) = LitBool b
lit exp = error $ "Unexpected not literal: " ++ show exp

letStmtName :: Ast.Stmt -> String
letStmtName (Ast.LetStmt (Tok _ name) _) = name
letStmtName stmt = error $ "Expected `LetStmt`, got: " ++ show stmt

isReturnStmt :: Ast.Stmt -> Bool
isReturnStmt (Ast.ReturnStmt _) = True
isReturnStmt _ = False

isLetStmt :: Ast.Stmt -> Bool
isLetStmt (Ast.LetStmt _ _) = True
isLetStmt _ = False

program :: String -> Ast.Program
program src = case parseProgram src of
  Left err -> error ("Parser error: " ++ err)
  Right program -> program

stmts :: String -> [Ast.Stmt]
stmts src = case program src of (Ast.Program xs) -> xs

singleExpr :: String -> Ast.Expr
singleExpr src = case stmt src of
  (Ast.ExprStmt expr) -> expr
  stmt -> error $ "Expected Ast.ExprStmt, got: " ++ show stmt

stmt :: String -> Ast.Stmt
stmt src = case stmts src of
  [s] -> s
  stms -> error $ "Expected 1 Ast.Stmt, got: " ++ show (length stms)

