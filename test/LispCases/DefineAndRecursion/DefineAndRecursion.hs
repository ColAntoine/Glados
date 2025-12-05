module LispCases.DefineAndRecursion.DefineAndRecursion (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Define and Recursion" spec

spec :: Spec
spec = do
    it "should handle simple define" $ do
        let result = evalLisp "(define x 5) x"
        result `shouldBe` Right "5\n"
    
    it "should handle define with expression" $ do
        let result = evalLisp "(define y (+ 2 3)) y"
        result `shouldBe` Right "5\n"
    
    it "should handle multiple defines" $ do
        let result = evalLisp "(define a 1) (define b 2) (+ a b)"
        result `shouldBe` Right "3\n"
    
    it "should handle define function" $ do
        let result = evalLisp "(define (add x y) (+ x y)) (add 3 4)"
        result `shouldBe` Right "7\n"
    
    it "should handle recursive function" $ do
        let result = evalLisp "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)"
        result `shouldBe` Right "120\n"
    
    it "should handle define with subtraction" $ do
        let result = evalLisp "(define (sub x y) (- x y)) (sub 10 3)"
        result `shouldBe` Right "7\n"
    
    it "should handle nested define calls" $ do
        let result = evalLisp "(define (double x) (* x 2)) (define (quad x) (double (double x))) (quad 5)"
        result `shouldBe` Right "20\n"
    
    it "should handle define with comparison" $ do
        let result = evalLisp "(define (gt x y) (< y x)) (gt 10 5)"
        result `shouldBe` Right "#t\n"
    
    it "should handle define overriding" $ do
        let result = evalLisp "(define x 1) (define x 2) x"
        result `shouldBe` Right "2\n"
    
    it "should fail on invalid define form" $ do
        let result = evalLisp "(define)"
        result `shouldSatisfy` isLeft
    
    it "should fail on define with invalid parameter" $ do
        let result = evalLisp "(define (f 5) x)"
        result `shouldSatisfy` isLeft
    
    it "should handle define using previous bindings" $ do
        let result = evalLisp "(define x 5) (define y (+ x 3)) y"
        result `shouldBe` Right "8\n"
    
    it "should handle define with lambda" $ do
        let result = evalLisp "(define f (lambda (x) (* x 2))) (f 6)"
        result `shouldBe` Right "12\n"

isLeft :: Either String String -> Bool
isLeft (Left _) = True
isLeft _ = False
