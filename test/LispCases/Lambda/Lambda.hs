module LispCases.Lambda.Lambda (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Lambda" spec

spec :: Spec
spec = do
    it "should handle lambda1.scm" $ do
        schemeCode <- readFile "test/LispCases/Lambda/lambda1.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
    
    it "should handle lambda2.scm" $ do
        schemeCode <- readFile "test/LispCases/Lambda/lambda2.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
    
    it "should handle lambda3.scm" $ do
        schemeCode <- readFile "test/LispCases/Lambda/lambda3.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
