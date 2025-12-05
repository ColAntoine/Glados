{-
-- EPITECH PROJECT, 2025
-- AST Test
-- File description:
-- Unit tests for AST data types
-}

module AST.AST (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import AST

tests :: IO TestTree
tests = testSpec "AST" spec

spec :: Spec
spec = do
    describe "Expr" $ do
        describe "EInt" $ do
            it "should create positive integer" $ do
                let expr = EInt 42
                expr `shouldBe` EInt 42
            
            it "should create zero" $ do
                let expr = EInt 0
                expr `shouldBe` EInt 0
            
            it "should create negative integer" $ do
                let expr = EInt (-100)
                expr `shouldBe` EInt (-100)
            
            it "should display as string" $ do
                let expr = EInt 123
                show expr `shouldBe` "EInt 123"

        describe "EBool" $ do
            it "should create true" $ do
                let expr = EBool True
                expr `shouldBe` EBool True
            
            it "should create false" $ do
                let expr = EBool False
                expr `shouldBe` EBool False
            
            it "should not equal opposite boolean" $ do
                let t = EBool True
                let f = EBool False
                t `shouldNotBe` f
            
            it "should display as string" $ do
                let expr = EBool True
                show expr `shouldBe` "EBool True"

        describe "ESymbol" $ do
            it "should create simple symbol" $ do
                let expr = ESymbol "x"
                expr `shouldBe` ESymbol "x"
            
            it "should create complex symbol" $ do
                let expr = ESymbol "my-variable"
                expr `shouldBe` ESymbol "my-variable"
            
            it "should create operator symbol" $ do
                let expr = ESymbol "+"
                expr `shouldBe` ESymbol "+"
            
            it "should distinguish different symbols" $ do
                let x = ESymbol "x"
                let y = ESymbol "y"
                x `shouldNotBe` y
            
            it "should display as string" $ do
                let expr = ESymbol "hello"
                show expr `shouldBe` "ESymbol \"hello\""

        describe "EList" $ do
            it "should create empty list" $ do
                let expr = EList []
                expr `shouldBe` EList []
            
            it "should create single element list" $ do
                let expr = EList [EInt 42]
                expr `shouldBe` EList [EInt 42]
            
            it "should create multiple element list" $ do
                let expr = EList [EInt 1, EInt 2, EInt 3]
                expr `shouldBe` EList [EInt 1, EInt 2, EInt 3]
            
            it "should create nested list" $ do
                let expr = EList [EList [EInt 1], EList [EInt 2]]
                expr `shouldBe` EList [EList [EInt 1], EList [EInt 2]]
            
            it "should create mixed type list" $ do
                let expr = EList [EInt 1, EBool True, ESymbol "x"]
                expr `shouldBe` EList [EInt 1, EBool True, ESymbol "x"]
            
            it "should display as string" $ do
                let expr = EList [EInt 1, EInt 2]
                show expr `shouldBe` "EList [EInt 1,EInt 2]"

    describe "Expr equality" $ do
        it "should equal identical integers" $ do
            EInt 42 `shouldBe` EInt 42
        
        it "should not equal different integers" $ do
            EInt 42 `shouldNotBe` EInt 43
        
        it "should equal identical booleans" $ do
            EBool True `shouldBe` EBool True
        
        it "should not equal different types" $ do
            EInt 1 `shouldNotBe` EBool True
        
        it "should equal identical symbols" $ do
            ESymbol "test" `shouldBe` ESymbol "test"
        
        it "should equal identical lists" $ do
            EList [EInt 1, EInt 2] `shouldBe` EList [EInt 1, EInt 2]
        
        it "should not equal different length lists" $ do
            EList [EInt 1] `shouldNotBe` EList [EInt 1, EInt 2]

    describe "Expr composition" $ do
        it "should create function call structure" $ do
            let expr = EList [ESymbol "+", EInt 1, EInt 2]
            expr `shouldBe` EList [ESymbol "+", EInt 1, EInt 2]
        
        it "should create nested function calls" $ do
            let expr = EList [ESymbol "+", EList [ESymbol "*", EInt 2, EInt 3], EInt 4]
            expr `shouldBe` EList [ESymbol "+", EList [ESymbol "*", EInt 2, EInt 3], EInt 4]
        
        it "should create lambda expression structure" $ do
            let expr = EList [ESymbol "lambda", EList [ESymbol "x"], ESymbol "x"]
            expr `shouldBe` EList [ESymbol "lambda", EList [ESymbol "x"], ESymbol "x"]
        
        it "should create define expression structure" $ do
            let expr = EList [ESymbol "define", ESymbol "x", EInt 10]
            expr `shouldBe` EList [ESymbol "define", ESymbol "x", EInt 10]
        
        it "should create if expression structure" $ do
            let expr = EList [ESymbol "if", EBool True, EInt 1, EInt 2]
            expr `shouldBe` EList [ESymbol "if", EBool True, EInt 1, EInt 2]

    describe "Pattern matching" $ do
        it "should match integer patterns" $ do
            case EInt 42 of
                EInt n -> n `shouldBe` 42
                _ -> fail "Should match EInt"
        
        it "should match boolean patterns" $ do
            case EBool False of
                EBool b -> b `shouldBe` False
                _ -> fail "Should match EBool"
        
        it "should match symbol patterns" $ do
            case ESymbol "test" of
                ESymbol s -> s `shouldBe` "test"
                _ -> fail "Should match ESymbol"
        
        it "should match list patterns" $ do
            case EList [EInt 1, EInt 2] of
                EList exprs -> length exprs `shouldBe` 2
                _ -> fail "Should match EList"
        
        it "should match cons pattern in list" $ do
            case EList [EInt 1, EInt 2, EInt 3] of
                EList (x:xs) -> do
                    x `shouldBe` EInt 1
                    length xs `shouldBe` 2
                _ -> fail "Should match cons pattern"

    describe "Expr large values" $ do
        it "should handle very large integers" $ do
            let expr = EInt 999999999999999999
            expr `shouldBe` EInt 999999999999999999
        
        it "should handle deep nesting" $ do
            let deepList = foldr (\i acc -> EList [EInt i, acc]) (EInt 0) [1..100]
            deepList `shouldNotBe` EInt 0
        
        it "should handle large lists" $ do
            let bigList = EList (replicate 1000 (EInt 1))
            length (case bigList of EList xs -> xs; _ -> []) `shouldBe` 1000
        
        it "should handle mixed deep structure" $ do
            let expr = EList [EList [EList [ESymbol "x"]], EBool True]
            show expr `shouldContain` "EList"
