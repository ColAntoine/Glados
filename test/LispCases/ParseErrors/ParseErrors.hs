module LispCases.ParseErrors.ParseErrors (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Parse Errors" spec

spec :: Spec
spec = do
    it "should fail on unmatched open paren" $ do
        let result = evalLisp "(+ 1 2"
        result `shouldSatisfy` isLeft
    
    it "should fail on unmatched close paren" $ do
        let result = evalLisp "+ 1 2)"
        result `shouldSatisfy` isLeft
    
    it "should parse empty input as empty program" $ do
        let result = evalLisp ""
        result `shouldBe` Right "\n"
    
    it "should parse whitespace only as empty program" $ do
        let result = evalLisp "   \n   "
        result `shouldBe` Right "\n"
    
    it "should fail on unbalanced parens" $ do
        let result = evalLisp "((+ 1 2)"
        result `shouldSatisfy` isLeft

isLeft :: Either String String -> Bool
isLeft (Left _) = True
isLeft _ = False
