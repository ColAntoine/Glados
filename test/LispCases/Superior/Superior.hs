module LispCases.Superior.Superior (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Superior" spec

spec :: Spec
spec = do
    it "should handle superior.scm" $ do
        schemeCode <- readFile "test/LispCases/Superior/superior.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
