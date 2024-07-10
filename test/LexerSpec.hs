module LexerSpec (spec) where

import Test.Hspec
import Lexer

spec :: Spec
spec = do
  describe "add 1" $ do
    it "it works" $ do
      add1 2 `shouldBe` (3 :: Int)
