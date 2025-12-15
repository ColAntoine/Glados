module LispCases.EdgeCases.EdgeCases (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Edge Cases" spec

spec :: Spec
spec = do
    it "should handle negative numbers" $ do
        let result = evalLisp "-42"
        result `shouldSatisfy` isRight
    
    it "should handle negative numbers in expressions" $ do
        let result = evalLisp "(+ -5 10)"
        result `shouldBe` Right "5\n"
    
    it "should handle zero" $ do
        let result = evalLisp "0"
        result `shouldBe` Right "0\n"
    
    it "should handle multiple spaces" $ do
        let result = evalLisp "(   +    1    2   )"
        result `shouldBe` Right "3\n"
    
    it "should handle empty list" $ do
        let result = evalLisp "'()"
        result `shouldBe` Right "'\n()\n"
    
    it "should handle nested lists" $ do
        let result = evalLisp "'(1 (2 (3)))"
        result `shouldBe` Right "'\n(1 (2 (3)))\n"
    
    it "should handle subtraction with multiple args" $ do
        let result = evalLisp "(- 10 2 3)"
        result `shouldBe` Right "5\n"
    
    it "should handle subtraction with one arg (negation)" $ do
        let result = evalLisp "(- 5)"
        result `shouldBe` Right "-5\n"
    
    it "should handle multiplication by zero" $ do
        let result = evalLisp "(* 5 0)"
        result `shouldBe` Right "0\n"
    
    it "should handle modulo operation" $ do
        let result = evalLisp "(mod 10 3)"
        result `shouldBe` Right "1\n"
    
    it "should handle modulo with negative" $ do
        let result = evalLisp "(mod -10 3)"
        result `shouldBe` Right "2\n"
    
    it "should handle multiple expressions" $ do
        let result = evalLisp "(+ 1 2) (+ 3 4)"
        result `shouldBe` Right "3\n7\n"
    
    it "should handle comparison operators" $ do
        let result = evalLisp "(< 5 10)"
        result `shouldBe` Right "#t\n"
    
    it "should handle false comparison" $ do
        let result = evalLisp "(< 10 5)"
        result `shouldBe` Right "#f\n"
    
    it "should handle equality" $ do
        let result = evalLisp "(= 5 5)"
        result `shouldBe` Right "#t\n"
    
    it "should handle inequality with eq?" $ do
        let result = evalLisp "(eq? 5 6)"
        result `shouldBe` Right "#f\n"
    
    it "should handle boolean true" $ do
        let result = evalLisp "#t"
        result `shouldBe` Right "#t\n"
    
    it "should handle boolean false" $ do
        let result = evalLisp "#f"
        result `shouldBe` Right "#f\n"
    
    it "should handle long symbol names" $ do
        let result = evalLisp "'very-long-symbol-name-here"
        result `shouldBe` Right "'VERY-LONG-SYMBOL-NAME-HERE\n"
    
    it "should handle symbols with numbers" $ do
        let result = evalLisp "'symbol123"
        result `shouldBe` Right "'SYMBOL123\n"

isRight :: Either String String -> Bool
isRight (Right _) = True
isRight _ = False
