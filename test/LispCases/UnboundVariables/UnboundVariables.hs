module LispCases.UnboundVariables.UnboundVariables (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Unbound Variables" spec

spec :: Spec
spec = do
    it "should fail on unbound variable in evaluation" $ do
        let result = evalLisp "(+ x 5)"
        result `shouldSatisfy` isLeft
    
    it "should fail on unbound in comparison" $ do
        let result = evalLisp "(< a 10)"
        result `shouldSatisfy` isLeft
    
    it "should handle quoted unbound symbols" $ do
        let result = evalLisp "'x"
        result `shouldBe` Right "'X\n"
    
    it "should fail when calling unbound function" $ do
        let result = evalLisp "(undefined-func 1 2 3)"
        result `shouldSatisfy` isLeft
    
    it "should fail with unbound in nested expression" $ do
        let result = evalLisp "(+ 1 (+ 2 unbound))"
        result `shouldSatisfy` isLeft
    
    it "should fail on unbound parameter in user function" $ do
        let result = evalLisp "(define (f x) (+ x y)) (f 5)"
        result `shouldSatisfy` isLeft
    
    it "should handle value passed to lambda" $ do
        let result = evalLisp "((lambda (x) x) 42)"
        result `shouldBe` Right "42\n" 
    
    it "should fail on unbound in lambda body" $ do
        let result = evalLisp "((lambda (x) (+ x y)) 5)"
        result `shouldSatisfy` isLeft
    
    it "should output unbound symbol from quote" $ do
        let result = evalLisp "'unbound-symbol"
        result `shouldBe` Right "'UNBOUND-SYMBOL\n"

isLeft :: Either String String -> Bool
isLeft (Left _) = True
isLeft _ = False
