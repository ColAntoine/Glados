module LispCases.RuntimeErrors.RuntimeErrors (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Runtime Errors" spec

spec :: Spec
spec = do
    it "should fail on undefined variable in arithmetic" $ do
        let result = evalLisp "(+ x 5)"
        result `shouldSatisfy` isLeft
    
    it "should fail on undefined variable in comparison" $ do
        let result = evalLisp "(< undefined 10)"
        result `shouldSatisfy` isLeft
    
    it "should fail on undefined function" $ do
        let result = evalLisp "(unknown-func 1 2)"
        result `shouldSatisfy` isLeft
    
    it "should handle no args to +" $ do
        let result = evalLisp "(+)"
        result `shouldBe` Right "0\n"
    
    it "should fail on wrong type in arithmetic" $ do
        let result = evalLisp "(+ #t 5)"
        result `shouldSatisfy` isLeft
    
    it "should fail on wrong arity for div" $ do
        let result = evalLisp "(div 10)"
        result `shouldSatisfy` isLeft
    
    it "should fail on wrong arity for comparison" $ do
        let result = evalLisp "(< 5)"
        result `shouldSatisfy` isLeft
    
    it "should fail on if with wrong arity" $ do
        let result = evalLisp "(if #t 1)"
        result `shouldSatisfy` isLeft
    
    it "should fail on invalid if condition with unbound variable" $ do
        let result = evalLisp "(if unbound-var 1 2)"
        result `shouldSatisfy` isLeft
    
    it "should fail on if with non-boolean unbound" $ do
        let result = evalLisp "(if undefined 1 2)"
        result `shouldSatisfy` isLeft
    
    it "should fail on if with non-boolean number" $ do
        let result = evalLisp "(if 5 1 2)"
        result `shouldSatisfy` isLeft
    
    it "should fail on if with numeric expression condition" $ do
        let result = evalLisp "(if (+ 1 2) 1 2)"
        result `shouldSatisfy` isLeft

isLeft :: Either String String -> Bool
isLeft (Left _) = True
isLeft _ = False
