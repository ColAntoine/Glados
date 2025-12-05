module LispCases.Function.Function (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Function" spec

spec :: Spec
spec = do
    it "should handle function1.scm" $ do
        schemeCode <- readFile "test/LispCases/Function/function1.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
