module ParserSpec (spec) where

import Test.Hspec
import Parser
import Ast
import Token

spec :: Spec
spec = do
  describe "let statements" $ do
    it "should parse correct ident names" $ do
      letStmtNames (validProgram input) `shouldBe` ["x", "y", "foobar"]
        where input = "let x = 5; let y = 10; let foobar = 838383;"

-- helpers

letStmtName :: Stmt -> String
letStmtName (LetStmt (T _ name) _) = name
letStmtName stmt = error "Expected `LetStmt`, got: " ++ show stmt

letStmtNames :: Program -> [String]
letStmtNames (Program stmts) = map letStmtName stmts

validProgram :: String -> Program
validProgram src = case parseProgram src of
  Left err -> error ("Parser error: " ++ err)
  Right p -> p

