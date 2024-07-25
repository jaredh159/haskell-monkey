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
    let input = "foo; bar; baz"
    fmap identExprName (stmts input) `shouldBe` ["foo", "bar", "baz"]

  it "should parse integer literal expressions" $ do
    let input = "5; 1000; 12345"
    fmap intLitExprVal (stmts input) `shouldBe` [5, 1000, 12345]

  it "should parse prefix expressions" $ do
    let input = "!5; -15;"
    let expected = [(Ast.PrefixBang, 5), (Ast.PrefixMinus, 15)]
    fmap prefixPair (stmts input) `shouldBe` expected

  it "should parse infix expressions" $ do
    let input = "5 + 5; 5 - 5; 5 / 5; 5 > 5; 5 < 5; 5 == 5; 5 != 5"
    let expected = [Ast.InfixPlus, Ast.InfixMinus, Ast.InfixSlash,
                    Ast.InfixGt, Ast.InfixLt, Ast.InfixEq, Ast.InfixNotEq]
    fmap infixTriple (stmts input) `shouldBe` map (\i -> (5, i, 5)) expected

  it "should handle precedence correctly" $ do
    let cases =
         [ ("-a * b", "((-a) * b)")
         , ("!-a", "(!(-a))")
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

infixTriple :: Ast.Stmt -> (Int, Ast.InfixOp, Int)
infixTriple (Ast.ExprStmt (Ast.Infix (Ast.IntLiteral l) op (Ast.IntLiteral r)))
  = (l, op, r)
infixTriple stmt = error ("Unexpected stmt: " ++ show stmt)

prefixPair :: Ast.Stmt -> (Ast.PrefixOp, Int)
prefixPair (Ast.ExprStmt (Ast.Prefix op (Ast.IntLiteral n))) = (op, n)

letStmtName :: Ast.Stmt -> String
letStmtName (Ast.LetStmt (Tok _ name) _) = name
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

program :: String -> Ast.Program
program src = case parseProgram src of
  Left err -> error ("Parser error: " ++ err)
  Right program -> program

stmts :: String -> [Ast.Stmt]
stmts src = case program src of (Ast.Program xs) -> xs


