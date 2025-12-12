module LispCases.SpecialCases.SpecialCases (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Special Cases" spec

spec :: Spec
spec = do
    it "should handle large numbers" $ do
        let result = evalLisp "1000000000"
        result `shouldBe` Right "1000000000\n"
    
    it "should handle very large arithmetic" $ do
        let result = evalLisp "(+ 999999999 1)"
        result `shouldBe` Right "1000000000\n"
    
    it "should handle chained comparisons" $ do
        let result = evalLisp "(< 1 2)"
        result `shouldBe` Right "#t\n"
    
    it "should handle nested if statements" $ do
        let result = evalLisp "(if #t (if #f 1 2) 3)"
        result `shouldBe` Right "2\n"
    
    it "should handle deeply nested arithmetic" $ do
        let result = evalLisp "(+ (+ (+ (+ 1 1) 1) 1) 1)"
        result `shouldBe` Right "5\n"
    
    it "should handle define then use immediately" $ do
        let result = evalLisp "(define x (+ 1 1)) (+ x x)"
        result `shouldBe` Right "4\n"
    
    it "should handle multiple arguments to primitives" $ do
        let result = evalLisp "(+ 1 2 3 4 5 6 7 8 9 10)"
        result `shouldBe` Right "55\n"
    
    it "should handle all comparison edge cases" $ do
        let result = evalLisp "(= (- 5) -5)"
        result `shouldBe` Right "#t\n"
    
    it "should handle list with define function" $ do
        let result = evalLisp "(define (sum a b c) (+ a b c)) (sum 1 2 3)"
        result `shouldBe` Right "6\n"
    
    it "should handle lambda capturing environment" $ do
        let result = evalLisp "(define x 10) ((lambda (y) (+ x y)) 5)"
        result `shouldBe` Right "15\n"
    
    it "should handle zero edge cases" $ do
        let result = evalLisp "(* 0 999)"
        result `shouldBe` Right "0\n"
    
    it "should handle single arg functions" $ do
        let result = evalLisp "(- 5)"
        result `shouldBe` Right "-5\n"
    
    it "should handle product of many numbers" $ do
        let result = evalLisp "(* 2 2 2 2 2)"
        result `shouldBe` Right "32\n"
    
    it "should handle equality with different types converted" $ do
        let result = evalLisp "(= 0 0)"
        result `shouldBe` Right "#t\n"
    
    it "should handle complex recursion" $ do
        let result = evalLisp "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 6)"
        result `shouldBe` Right "8\n"
