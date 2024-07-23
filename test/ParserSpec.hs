module ParserSpec (spec) where

import Test.Hspec
import Parser
import qualified Ast
import Token

spec :: Spec
spec = do
  it "should parse correct ident names" $ do
    let input = "let x = 5; let y = 10; let foobar = 838383;"
    map letStmtName (validProgram input) `shouldBe` ["x", "y", "foobar"]

  it "should recognize return statments" $ do
    let input = "return 5; return 10; return 993322;"
    all isReturnStmt (validProgram input) `shouldBe` True

  it "should parse identifier expressions" $ do
    let input = "foo; bar; baz"
    fmap identExprName (validProgram input) `shouldBe` ["foo", "bar", "baz"]

  it "should parse integer literal expressions" $ do
    let input = "5; 1000; 12345"
    fmap intLitExprVal (validProgram input) `shouldBe` [5, 1000, 12345]

  it "should parse prefix expressions" $ do
    let input = "!5; -15;"
    let expected = [(Ast.PrefixBang, 5), (Ast.PrefixMinus, 15)]
    fmap prefixPair (validProgram input) `shouldBe` expected

-- helpers

prefixPair :: Ast.Stmt -> (Ast.PrefixOp, Int)
prefixPair (Ast.ExprStmt (Ast.Prefix op (Ast.IntLiteral n))) = (op, n)

letStmtName :: Ast.Stmt -> String
letStmtName (Ast.LetStmt (T _ name) _) = name
letStmtName stmt = error $ "Expected `LetStmt`, got: " ++ show stmt

identExprName :: Ast.Stmt -> String
identExprName (Ast.ExprStmt (Ast.Ident name)) = name
identExprName stmt = error $ "Expected `ExprStmt`, got: " ++ show stmt

intLitExprVal :: Ast.Stmt -> Int
intLitExprVal (Ast.ExprStmt (Ast.IntLiteral val)) = val
intLitExprVal stmt = error $ "Expected `IntLiteral`, got: " ++ show stmt

isReturnStmt :: Ast.Stmt -> Bool
isReturnStmt (Ast.ReturnStmt _) = True
isReturnStmt _ = False

isLetStmt :: Ast.Stmt -> Bool
isLetStmt (Ast.LetStmt _ _) = True
isLetStmt _ = False

validProgram :: String -> [Ast.Stmt]
validProgram src = case parseProgram src of
  Left err -> error ("Parser error: " ++ err)
  Right (Ast.Program stmts) -> stmts

