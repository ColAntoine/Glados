module ParserCases.Parser.Parser (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Parser.LISP (parseProgram)
import AST (Expr(..))

tests :: IO TestTree
tests = testSpec "Parser" spec

spec :: Spec
spec = do
  it "parses a simple division list" $ do
    parseProgram "(div 10 2)" `shouldBe` Right [EList [ESymbol "div", EInt 10, EInt 2]]

  it "parses booleans" $ do
    parseProgram "#t" `shouldBe` Right [EBool True]
    parseProgram "#f" `shouldBe` Right [EBool False]

  it "parses numbers and symbols" $ do
    parseProgram "42" `shouldBe` Right [EInt 42]
    parseProgram "foo" `shouldBe` Right [ESymbol "foo"]

  it "parses nested lists" $ do
    parseProgram "(add (mul 2 3) 4)" `shouldBe` Right [EList [ESymbol "add", EList [ESymbol "mul", EInt 2, EInt 3], EInt 4]]
