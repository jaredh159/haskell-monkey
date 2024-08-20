module EvalSpec (spec) where

import Test.Hspec
import qualified Data.Map as M

import Env (Object(..))
import qualified Ast
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
    eval "\"hello world\"" `shouldBe` ObjString "hello world"

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
    eval "5 - 4" `shouldBe` ObjInt 1
    eval "5 + 4 * 3" `shouldBe` ObjInt 17
    eval "(5 + 10 * 2 + 15 / 3) * 2 + -10" `shouldBe` ObjInt 50
    eval "50 / 2 * 2 + 10" `shouldBe` ObjInt 60
    eval "1 < 2" `shouldBe` ObjBool True
    eval "1 > 2" `shouldBe` ObjBool False
    eval "1 < 1" `shouldBe` ObjBool False
    eval "1 > 1" `shouldBe` ObjBool False
    eval "1 == 1" `shouldBe` ObjBool True
    eval "1 != 1" `shouldBe` ObjBool False
    eval "1 == 2" `shouldBe` ObjBool False
    eval "1 != 2" `shouldBe` ObjBool True
    eval "true == true" `shouldBe` ObjBool True
    eval "false == false" `shouldBe` ObjBool True
    eval "true == false" `shouldBe` ObjBool False
    eval "true != false" `shouldBe` ObjBool True
    eval "\"foo\" + \"bar\"" `shouldBe` ObjString "foobar"

  it "should evaluate if else expressions" $ do
    eval "if (true) { 10 }" `shouldBe` ObjInt 10
    eval "if (false) { 10 }" `shouldBe` ObjNull
    eval "if (1) { 10 }" `shouldBe` ObjInt 10
    eval "if (1 < 2) { 10 }" `shouldBe` ObjInt 10
    eval "if (1 > 2) { 10 }" `shouldBe` ObjNull
    eval "if (1 > 2) { 10 } else { 20 }" `shouldBe` ObjInt 20
    eval "if (1 < 2) { 10 } else { 20 }" `shouldBe` ObjInt 10

  it "should evaluate return expressions" $ do
    eval "return 10;" `shouldBe` ObjInt 10
    eval "return 10; 9;" `shouldBe` ObjInt 10
    eval "return 2 * 5; 9;" `shouldBe` ObjInt 10
    eval "9; return 2 * 5; 9;" `shouldBe` ObjInt 10
    eval "if (2>1) { if (2>1) { return 10; } return 9; }" `shouldBe` ObjInt 10

  it "should evaluate let expressions" $ do
    eval "let a = 5; a;" `shouldBe` ObjInt 5
    eval "let a = 5 * 5; a;" `shouldBe` ObjInt 25
    eval "let a = 5; let b = a; b" `shouldBe` ObjInt 5
    eval "let a = 5; let b = a; let c = a + b + 5; c;" `shouldBe` ObjInt 15

  it "should evaluate function expressions" $ do
    let reduce (ObjFn params body _) = (params, Ast.stringify body)
    reduce (eval "fn(x) { x + 2; };") `shouldBe` (["x"], "(x + 2)")
    eval "let id = fn(x) { x; }; id(5);" `shouldBe` ObjInt 5
    eval "let id = fn(x) { return x; }; id(5);" `shouldBe` ObjInt 5
    eval "let double = fn(x) { x * 2; }; double(5);" `shouldBe` ObjInt 10
    eval "let add = fn(x, y) { x + y; }; add(5, 5);" `shouldBe` ObjInt 10
    eval "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));" `shouldBe` ObjInt 20
    eval "fn(x) { x; }(5)" `shouldBe` ObjInt 5
    eval "let a = fn(x){fn(y) {x+y};}; let b = a(2); b(2);" `shouldBe` ObjInt 4

  it "should evaluate builtin functions" $ do
    eval "len(\"\")" `shouldBe` ObjInt 0
    eval "len(\"foo\")" `shouldBe` ObjInt 3
    eval "len([])" `shouldBe` ObjInt 0
    eval "len([1, 2, 3])" `shouldBe` ObjInt 3
    eval "first([1, 2, 3])" `shouldBe` ObjInt 1
    eval "first([])" `shouldBe` ObjNull
    eval "last([1, 2, 3])" `shouldBe` ObjInt 3
    eval "last([])" `shouldBe` ObjNull
    eval "rest([1, 2, 3])" `shouldBe` ObjArray [ObjInt 2, ObjInt 3]
    eval "rest([1, 2])" `shouldBe` ObjArray [ObjInt 2]
    eval "rest([])" `shouldBe` ObjNull
    eval "push([], 1)" `shouldBe` ObjArray [ObjInt 1]
    eval "push([1], 2)" `shouldBe` ObjArray [ObjInt 1, ObjInt 2]

  it "should evaluate recursive functions" $ do
    let prog = "let fb = fn(x) { if (x == 0) {0} else {x + fb(x - 1)} }; fb(5);"
    eval prog `shouldBe` ObjInt 15

  it "should evaluate mutually recursive functions" $ do
    let isEven = "let isEven = fn(n) { if (n == 0) { true } else { isOdd(n - 1) } };"
    let isOdd = "let isOdd = fn(n) { if (n == 0) { false } else { isEven(n - 1) } };"
    eval (isEven ++ isOdd ++ "isEven(4)") `shouldBe` ObjBool True
    eval (isEven ++ isOdd ++ "isEven(5)") `shouldBe` ObjBool False
    eval (isEven ++ isOdd ++ "isOdd(4)") `shouldBe` ObjBool False

  it "should evaluate array literals" $ do
    eval "[1, 2 * 2, 3 + 3]" `shouldBe` ObjArray [ObjInt 1, ObjInt 4, ObjInt 6]

  it "should evaluate index expressions" $ do
    eval "[1, 2, 3][0]" `shouldBe` ObjInt 1
    eval "[1, 2, 3][1]" `shouldBe` ObjInt 2
    eval "[1, 2, 3][2]" `shouldBe` ObjInt 3
    eval "[1, 2, 3][3]" `shouldBe` ObjNull
    eval "[1, 2, 3][-1]" `shouldBe` ObjNull
    eval "[1, 2, 3][1 + 1]" `shouldBe` ObjInt 3
    eval "let a = [1, 2, 3]; a[1 + 1]" `shouldBe` ObjInt 3
    eval "let a = [1, 2, 3]; a[0] + a[1] + a[2]" `shouldBe` ObjInt 6
    eval "let a = [1, 2, 3]; let i = a[0]; a[i]" `shouldBe` ObjInt 2

  it "should evaluate hash literals" $ do
    let prog = "let two = \"two\"; {\"one\": 10 - 9, two: 1 + 1,\
      \\"thr\" + \"ee\": 6 / 2, 4:4, true: 5, false: 6}"
    eval prog `shouldBe` ObjHash (M.fromList
      [ (ObjString "one", ObjInt 1)
      , (ObjString "two", ObjInt 2)
      , (ObjString "three", ObjInt 3)
      , (ObjInt 4, ObjInt 4)
      , (ObjBool True, ObjInt 5)
      , (ObjBool False, ObjInt 6)])

  it "should evaluate hash index expressions" $ do
    eval "{\"foo\": 5}[\"foo\"]" `shouldBe` ObjInt 5
    eval "{\"foo\": 5}[\"bar\"]" `shouldBe` ObjNull
    eval "let k = \"foo\";{\"foo\": 5}[k]" `shouldBe` ObjInt 5
    eval "{}[\"foo\"]" `shouldBe` ObjNull
    eval "{5: 5}[5]" `shouldBe` ObjInt 5
    eval "{true: 5}[true]" `shouldBe` ObjInt 5
    eval "{false: 5}[false]" `shouldBe` ObjInt 5
    eval "let id = fn(x) {x}; {id: 5}[id]" `shouldBe` ObjInt 5
    eval "let id = fn(x) {x}; {id: 5}[fn(x) {x}]" `shouldBe` ObjNull
    eval "let id = fn(x) {x}; {id: 5}[fn(y) {y}]" `shouldBe` ObjNull

  it "should report errors" $ do
    evalE "5 + true" `shouldBe` "Type mismatch: INTEGER + BOOLEAN"
    evalE "5 + true; 5" `shouldBe` "Type mismatch: INTEGER + BOOLEAN"
    evalE "-true" `shouldBe` "Unknown operator: -BOOLEAN"
    evalE "true + false" `shouldBe` "Type mismatch: BOOLEAN + BOOLEAN"
    evalE "foobar" `shouldBe` "Identifier not found: `foobar`"
    evalE "\"foo\" - \"bar\"" `shouldBe` "Unknown operator: STRING - STRING"
    evalE "len(1)" `shouldBe` "Argument to `len` not supported, got INTEGER"
    evalE "len(1, 2)" `shouldBe` "Wrong num args, expected 1, got 2"

-- helpers

eval :: String -> Object
eval src = case Eval.eval (Ast.ProgNode (program src)) of
  Right obj -> obj
  Left err -> error $ "Eval ERROR: " ++ show err

evalE :: String -> String
evalE src = case Eval.eval (Ast.ProgNode (program src)) of
  Right obj -> error $ "Expected error, got: " ++ show obj
  Left err -> err


