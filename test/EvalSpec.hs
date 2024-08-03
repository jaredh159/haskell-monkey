module EvalSpec (spec) where

import Test.Hspec
import Object
import qualified Ast
import Parser
import qualified Eval
import ParserSpec (program)

spec :: Spec
spec = do
  it "should evaluate literal expressions" $ do
    eval "5" `shouldBe` ObjInt 5
    eval "10" `shouldBe` ObjInt 10
    eval "5; false; 15;" `shouldBe` ObjInt 15
    eval "true" `shouldBe` ObjBool True
    eval "false" `shouldBe` ObjBool False

  it "should evaluate prefix expressions" $ do
    eval "!true" `shouldBe` ObjBool False
    eval "!false" `shouldBe` ObjBool True
    eval "!5" `shouldBe` ObjBool False
    eval "!!true" `shouldBe` ObjBool True
    eval "!!false" `shouldBe` ObjBool False
    eval "!!5" `shouldBe` ObjBool True
    eval "-5" `shouldBe` ObjInt (-5)
    eval "-10" `shouldBe` ObjInt (-10)

  it "should evaluate infix expressions" $ do
    eval "1 + 2" `shouldBe` ObjInt 3
    eval "5 + 4" `shouldBe` ObjInt 9
    eval "5 + 4 * 3" `shouldBe` ObjInt 17
    eval "(5 + 10 * 2 + 15 / 3) * 2 + -10" `shouldBe` ObjInt 50
    eval "50 / 2 * 2 + 10" `shouldBe` ObjInt 60

-- helpers

eval :: String -> Object
eval src = case Eval.eval $ Ast.ProgNode $ program src of
  Right obj -> obj
  Left err -> error $ "Eval ERROR: " ++ show err

