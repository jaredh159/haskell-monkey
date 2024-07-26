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
    let triple (Ast.ExprStmt (Ast.Infix lhs op rhs)) = (lit lhs, op, lit rhs)
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
    map ((triple . stmt) . fst) cases `shouldBe` map snd cases


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
         ]
    map ((Ast.stringify . program) . fst) cases `shouldBe` map snd cases

-- helpers

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

stmt :: String -> Ast.Stmt
stmt src = case stmts src of
  [s] -> s
  stms -> error $ "Expected 1 Ast.Stmt, got: " ++ show (length stms)


