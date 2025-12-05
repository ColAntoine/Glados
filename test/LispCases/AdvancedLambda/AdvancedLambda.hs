module LispCases.AdvancedLambda.AdvancedLambda (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Advanced Lambda" spec

spec :: Spec
spec = do
    it "should handle lambda with zero args" $ do
        let result = evalLisp "((lambda () 42))"
        result `shouldBe` Right "42\n"
    
    it "should handle lambda with multiple args" $ do
        let result = evalLisp "((lambda (a b c) (+ a b c)) 1 2 3)"
        result `shouldBe` Right "6\n"
    
    it "should handle nested lambda" $ do
        let result = evalLisp "((lambda (x) (lambda (y) (+ x y))) 5)"
        result `shouldSatisfy` isRight
    
    it "should handle lambda returning lambda" $ do
        let result = evalLisp "(define add5 (lambda (x) (+ x 5))) (add5 10)"
        result `shouldBe` Right "15\n"
    
    it "should handle lambda evaluation" $ do
        let result = evalLisp "((lambda (x) (* x 2)) 5)"
        result `shouldBe` Right "10\n"
    
    it "should fail on lambda with wrong arity" $ do
        let result = evalLisp "((lambda (x y) (+ x y)) 1)"
        result `shouldSatisfy` isLeft
    
    it "should fail on lambda with too many args" $ do
        let result = evalLisp "((lambda (x) x) 1 2 3)"
        result `shouldSatisfy` isLeft
    
    it "should handle lambda with conditionals" $ do
        let result = evalLisp "((lambda (x) (if (< x 0) (- x) x)) -5)"
        result `shouldBe` Right "5\n"
    
    it "should handle composed lambdas" $ do
        let result = evalLisp "(define double (lambda (x) (* x 2))) (define add3 (lambda (x) (+ x 3))) (add3 (double 5))"
        result `shouldBe` Right "13\n"
    
    it "should handle higher order lambda" $ do
        let result = evalLisp "((lambda (f) (f 10)) (lambda (x) (* x 2)))"
        result `shouldBe` Right "20\n"
    
    it "should fail on invalid lambda body" $ do
        let result = evalLisp "(lambda (x))"
        result `shouldSatisfy` isLeft

isLeft :: Either String String -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either String String -> Bool
isRight (Right _) = True
isRight _ = False
