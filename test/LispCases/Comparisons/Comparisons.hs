module LispCases.Comparisons.Comparisons (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Comparisons" spec

spec :: Spec
spec = do
    it "should handle less than true" $ do
        let result = evalLisp "(< 1 2)"
        result `shouldBe` Right "#t\n"
    
    it "should handle less than false" $ do
        let result = evalLisp "(< 5 2)"
        result `shouldBe` Right "#f\n"
    
    it "should handle equal numbers with =" $ do
        let result = evalLisp "(= 5 5)"
        result `shouldBe` Right "#t\n"
    
    it "should handle unequal numbers with =" $ do
        let result = evalLisp "(= 5 6)"
        result `shouldBe` Right "#f\n"
    
    it "should handle equal with eq?" $ do
        let result = evalLisp "(eq? 42 42)"
        result `shouldBe` Right "#t\n"
    
    it "should handle boolean comparison" $ do
        let result = evalLisp "(= #t #t)"
        result `shouldBe` Right "#t\n"
    
    it "should handle boolean inequality" $ do
        let result = evalLisp "(= #t #f)"
        result `shouldBe` Right "#f\n"
    
    it "should handle less than with equal" $ do
        let result = evalLisp "(< 5 5)"
        result `shouldBe` Right "#f\n"
    
    it "should handle less than negative numbers" $ do
        let result = evalLisp "(< -10 -5)"
        result `shouldBe` Right "#t\n"
    
    it "should handle comparison with zero" $ do
        let result = evalLisp "(< 0 1)"
        result `shouldBe` Right "#t\n"
    
    it "should handle large number comparison" $ do
        let result = evalLisp "(< 999999 1000000)"
        result `shouldBe` Right "#t\n"
