module EvalSpec (spec) where

import Test.Hspec
import Object
import qualified Ast
import Parser
import Eval (eval)
import ParserSpec (program)

spec :: Spec
spec = do
  it "should evaluate integer expressions" $ do
    evalp "5" `shouldBe` ObjInt 5
    evalp "10" `shouldBe` ObjInt 10

-- helpers

evalp :: String -> Object
evalp src = eval $ Ast.ProgNode $ program src
