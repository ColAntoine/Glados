module LispCases.Call.Call (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Call" spec

spec :: Spec
spec = do
    it "should evaluate (div 10 2) to 5" $ do
        let result = evalLisp "(div 10 2)"
        result `shouldBe` Right "5\n"
