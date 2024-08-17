module LexerSpec (spec) where

import Prelude hiding (lex)
import Control.Monad.State
import Test.Hspec
import Lexer
import Token


spec :: Spec
spec = do
  it "should lex tokens" $ do
    let input = "let five2 = 5;\n\
              \let ten = 10;\n\
              \\n\
              \let add = fn(x, y) {\n\
              \  x + y;\n\
              \};\n\
              \\n\
              \let result = add(five, ten);\n\
              \!-/*5;\n\
              \5 < 10 > 5;\n\
              \true false if else return == !=;"
      in tokens input
        `shouldBe` [
                     token Let "let",
                     token Ident "five2",
                     token Assign "=",
                     token Int "5",
                     token SemiColon ";",
                     token Let "let",
                     token Ident "ten",
                     token Assign "=",
                     token Int "10",
                     token SemiColon ";",
                     token Let "let",
                     token Ident "add",
                     token Assign "=",
                     token Function "fn",
                     token LParen "(",
                     token Ident "x",
                     token Comma ",",
                     token Ident "y",
                     token RParen ")",
                     token LBrace "{",
                     token Ident "x",
                     token Plus "+",
                     token Ident "y",
                     token SemiColon ";",
                     token RBrace "}",
                     token SemiColon ";",
                     token Let "let",
                     token Ident "result",
                     token Assign "=",
                     token Ident "add",
                     token LParen "(",
                     token Ident "five",
                     token Comma ",",
                     token Ident "ten",
                     token RParen ")",
                     token SemiColon ";",
                     token Bang "!",
                     token Minus "-",
                     token Slash "/",
                     token Asterisk "*",
                     token Int "5",
                     token SemiColon ";",
                     token Int "5",
                     token Lt "<",
                     token Int "10",
                     token Gt ">",
                     token Int "5",
                     token SemiColon ";",
                     token MTrue "true",
                     token MFalse "false",
                     token If "if",
                     token Else "else",
                     token Return "return",
                     token Eq "==",
                     token NotEq "!=",
                     token SemiColon ";"
                   ]



