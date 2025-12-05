module LispCases.If.If (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "If" spec

spec :: Spec
spec = do
    it "should handle if1.scm" $ do
        schemeCode <- readFile "test/LispCases/If/if1.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
    
    it "should handle if2.scm" $ do
        schemeCode <- readFile "test/LispCases/If/if2.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
    
    it "should handle if3.scm" $ do
        schemeCode <- readFile "test/LispCases/If/if3.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
