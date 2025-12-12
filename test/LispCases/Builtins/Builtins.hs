module LispCases.Builtins.Builtins (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Builtins" spec

spec :: Spec
spec = do
    it "should handle builtins1.scm" $ do
        schemeCode <- readFile "test/LispCases/Builtins/builtins1.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
    
    it "should handle builtins2.scm" $ do
        schemeCode <- readFile "test/LispCases/Builtins/builtins2.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
    
    it "should handle builtins3.scm" $ do
        schemeCode <- readFile "test/LispCases/Builtins/builtins3.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
