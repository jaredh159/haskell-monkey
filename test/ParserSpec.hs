module ParserSpec (spec) where

import Test.Hspec
import Parser
import Ast
import Token
import Data.Maybe (fromJust)

spec :: Spec
spec = do
  describe "let statements" $ do
    it "works" $ do
      letStmtNames (validProgram input) `shouldBe` ["x", "y", "foobar"]
        where input = "let x = 5; let y = 10; let foobar = 838383;"

-- helpers

letStmtName :: Stmt -> String
letStmtName (LetStmt _ (T _ name) _) = name
letStmtName stmt = error "Expected `LetStmt`, got: " ++ show stmt

letStmtNames :: Program -> [String]
letStmtNames (Program stmts) = map letStmtName stmts

validProgram :: String -> Program
validProgram = fromJust . parseProgram

