module LispCases.ArithmeticOps.ArithmeticOps (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Arithmetic Operations" spec

spec :: Spec
spec = do
    it "should handle addition of multiple args" $ do
        let result = evalLisp "(+ 1 2 3 4 5)"
        result `shouldBe` Right "15\n"
    
    it "should handle zero addition" $ do
        let result = evalLisp "(+ 0)"
        result `shouldBe` Right "0\n"
    
    it "should handle subtraction of three numbers" $ do
        let result = evalLisp "(- 20 5 3)"
        result `shouldBe` Right "12\n"
    
    it "should handle multiplication of single arg" $ do
        let result = evalLisp "(* 7)"
        result `shouldBe` Right "7\n"
    
    it "should handle multiplication of many args" $ do
        let result = evalLisp "(* 2 3 4)"
        result `shouldBe` Right "24\n"
    
    it "should handle division" $ do
        let result = evalLisp "(div 100 10)"
        result `shouldBe` Right "10\n"
    
    it "should handle division with remainder" $ do
        let result = evalLisp "(div 17 5)"
        result `shouldBe` Right "3\n"
    
    it "should handle modulo" $ do
        let result = evalLisp "(mod 17 5)"
        result `shouldBe` Right "2\n"
    
    it "should fail on modulo by zero" $ do
        let result = evalLisp "(mod 5 0)"
        result `shouldSatisfy` isLeft
    
    it "should fail on division by zero" $ do
        let result = evalLisp "(div 5 0)"
        result `shouldSatisfy` isLeft
    
    it "should handle mixed arithmetic" $ do
        let result = evalLisp "(+ 1 (* 2 3) (- 10 5))"
        result `shouldBe` Right "12\n"
    
    it "should handle negative multiplication" $ do
        let result = evalLisp "(* -3 4)"
        result `shouldBe` Right "-12\n"
    
    it "should handle division of negative" $ do
        let result = evalLisp "(div -20 4)"
        result `shouldBe` Right "-5\n"

isLeft :: Either String String -> Bool
isLeft (Left _) = True
isLeft _ = False
