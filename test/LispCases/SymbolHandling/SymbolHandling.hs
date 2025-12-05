module LispCases.SymbolHandling.SymbolHandling (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Symbol Handling" spec

spec :: Spec
spec = do
    it "should handle single character symbol" $ do
        let result = evalLisp "'x"
        result `shouldBe` Right "'X\n"
    
    it "should handle multi-character symbol" $ do
        let result = evalLisp "'hello"
        result `shouldBe` Right "'HELLO\n"
    
    it "should handle symbol with hyphen" $ do
        let result = evalLisp "'my-var"
        result `shouldBe` Right "'MY-VAR\n"
    
    it "should handle symbol with numbers" $ do
        let result = evalLisp "'var123"
        result `shouldBe` Right "'VAR123\n"
    
    it "should handle symbol with multiple hyphens" $ do
        let result = evalLisp "'a-b-c-d-e"
        result `shouldBe` Right "'A-B-C-D-E\n"
    
    it "should handle operator symbols" $ do
        let result = evalLisp "'+'"
        result `shouldBe` Right "'+'\n"
    
    it "should handle define symbol" $ do
        let result = evalLisp "'define"
        result `shouldBe` Right "'DEFINE\n"
    
    it "should handle lambda symbol" $ do
        let result = evalLisp "'lambda"
        result `shouldBe` Right "'LAMBDA\n"
    
    it "should handle if symbol" $ do
        let result = evalLisp "'if"
        result `shouldBe` Right "'IF\n"
