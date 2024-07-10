module LexerSpec (spec) where

import Control.Monad.State
import Test.Hspec
import Lexer
import Token

spec :: Spec
spec = do
  describe "nextToken" $ do
    it "it works" $ do
      evalState nextToken (lexer "") `shouldBe` token Eof ""
