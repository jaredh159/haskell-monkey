module ParserSpec (spec) where

import Test.Hspec
import Parser
import Ast
import Token

spec :: Spec
spec = do
  describe "let statements" $ do
    it "should parse correct ident names" $ do
      let input = "let x = 5; let y = 10; let foobar = 838383;"
      map letStmtName (validProgram input) `shouldBe` ["x", "y", "foobar"]

    it "should recognize return statments" $ do
      let input = "return 5; return 10; return 993322;"
      all isReturnStmt (validProgram input) `shouldBe` True

-- helpers

letStmtName :: Stmt -> String
letStmtName (LetStmt (T _ name) _) = name
letStmtName stmt = error "Expected `LetStmt`, got: " ++ show stmt

isReturnStmt :: Stmt -> Bool
isReturnStmt (ReturnStmt _) = True
isReturnStmt _ = False

validProgram :: String -> [Stmt]
validProgram src = case parseProgram src of
  Left err -> error ("Parser error: " ++ err)
  Right (Program stmts) -> stmts

