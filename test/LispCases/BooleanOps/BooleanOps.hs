module LispCases.BooleanOps.BooleanOps (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Boolean Operations" spec

spec :: Spec
spec = do
    it "should handle true value" $ do
        let result = evalLisp "#t"
        result `shouldBe` Right "#t\n"
    
    it "should handle false value" $ do
        let result = evalLisp "#f"
        result `shouldBe` Right "#f\n"
    
    it "should handle true in if" $ do
        let result = evalLisp "(if #t 1 2)"
        result `shouldBe` Right "1\n"
    
    it "should handle false in if" $ do
        let result = evalLisp "(if #f 1 2)"
        result `shouldBe` Right "2\n"
    
    it "should handle true in list" $ do
        let result = evalLisp "'(#t #f #t)"
        result `shouldBe` Right "'\n(#t #f #t)\n"
    
    it "should handle boolean equality" $ do
        let result = evalLisp "(= #t #t)"
        result `shouldBe` Right "#t\n"
    
    it "should handle boolean inequality" $ do
        let result = evalLisp "(= #t #f)"
        result `shouldBe` Right "#f\n"
    
    it "should handle boolean in quoted form" $ do
        let result = evalLisp "'#t"
        result `shouldBe` Right "'#T\n"
    
    it "should compare true with eq?" $ do
        let result = evalLisp "(eq? #t #t)"
        result `shouldBe` Right "#t\n"
    
    it "should compare false with eq?" $ do
        let result = evalLisp "(eq? #f #f)"
        result `shouldBe` Right "#t\n"
