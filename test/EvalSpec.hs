module EvalSpec (spec) where

import Test.Hspec
import Object
import qualified Ast
import Parser
import Eval (eval)
import ParserSpec (program)

spec :: Spec
spec = do
  it "should evaluate literal expressions" $ do
    evalp "5" `shouldBe` ObjInt 5
    evalp "10" `shouldBe` ObjInt 10
    evalp "true" `shouldBe` ObjBool True
    evalp "false" `shouldBe` ObjBool False

  it "should evaluate prefix expressions" $ do
    evalp "!true" `shouldBe` ObjBool False
    evalp "!false" `shouldBe` ObjBool True
    evalp "!5" `shouldBe` ObjBool False
    evalp "!!true" `shouldBe` ObjBool True
    evalp "!!false" `shouldBe` ObjBool False
    evalp "!!5" `shouldBe` ObjBool True
    evalp "-5" `shouldBe` ObjInt (-5)
    evalp "-10" `shouldBe` ObjInt (-10)

-- helpers

evalp :: String -> Object
evalp src = eval $ Ast.ProgNode $ program src
