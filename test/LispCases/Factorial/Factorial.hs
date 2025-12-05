module LispCases.Factorial.Factorial (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Factorial" spec

spec :: Spec
spec = do
    it "should handle factorial.scm" $ do
        schemeCode <- readFile "test/LispCases/Factorial/factorial.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
