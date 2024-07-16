module LexerSpec (spec) where

import Prelude hiding (lex)
import Control.Monad.State
import Test.Hspec
import Lexer
import Token


tokens :: String -> State String (Maybe Token) -> [Token] -> [Token]
tokens input lexer acc = case runState lexer input of
  (Nothing, "") -> reverse acc
  (Nothing, rest) -> error ("Unused input: `" ++ rest ++ "`")
  (Just t, rest) -> tokens rest lexer (t:acc)


lex :: String -> [Token]
lex inp = tokens inp nextToken []

spec :: Spec
spec = do
  describe "nextToken" $ do
    it "it works" $ do
      lex "=+(){},;"
        `shouldBe` [ token Assign "=",
                     token Plus "+",
                     token LParen "(",
                     token RParen ")",
                     token LBrace "{",
                     token RBrace "}",
                     token Comma ",",
                     token SemiColon ";"
                   ]

