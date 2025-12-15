module LispCases.DataStructures.DataStructures (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Data Structures" spec

spec :: Spec
spec = do
    it "should handle simple quoted list" $ do
        let result = evalLisp "'(1 2 3)"
        result `shouldBe` Right "'\n(1 2 3)\n"
    
    it "should handle quoted empty list" $ do
        let result = evalLisp "'()"
        result `shouldBe` Right "'\n()\n"
    
    it "should handle quoted nested lists" $ do
        let result = evalLisp "'((1 2) (3 4) (5 6))"
        result `shouldBe` Right "'\n((1 2) (3 4) (5 6))\n"
    
    it "should handle quoted mixed types" $ do
        let result = evalLisp "'(1 #t abc)"
        result `shouldBe` Right "'\n(1 #t ABC)\n"
    
    it "should handle list with booleans" $ do
        let result = evalLisp "'(#t #f #t)"
        result `shouldBe` Right "'\n(#t #f #t)\n"
    
    it "should handle deeply nested quoted list" $ do
        let result = evalLisp "'(1 (2 (3 (4 (5)))))"
        result `shouldBe` Right "'\n(1 (2 (3 (4 (5)))))\n"
    
    it "should handle quoted list with negative numbers" $ do
        let result = evalLisp "'(-1 -2 -3)"
        result `shouldBe` Right "'\n(-1 -2 -3)\n"
    
    it "should handle quoted list of numbers" $ do
        let result = evalLisp "'(1 2 3 4 5)"
        result `shouldBe` Right "'\n(1 2 3 4 5)\n"
    
    it "should handle very large quoted list" $ do
        let result = evalLisp "'(1 2 3 4 5 6 7 8 9 10)"
        result `shouldBe` Right "'\n(1 2 3 4 5 6 7 8 9 10)\n"
    
    it "should handle single element list" $ do
        let result = evalLisp "'(42)"
        result `shouldBe` Right "'\n(42)\n"
