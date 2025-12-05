module LispCases.Foo.Foo (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Foo" spec

spec :: Spec
spec = do
    it "should handle foo.scm" $ do
        schemeCode <- readFile "test/LispCases/Foo/foo.scm"
        let result = evalLisp schemeCode
        result `shouldNotBe` Left ""
