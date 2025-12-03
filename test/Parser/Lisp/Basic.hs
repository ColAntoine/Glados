module Parser.Lisp.Basic (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Parser.LISP (parseProgram)
import AST (Expr(..))

tests :: IO TestTree
tests = testSpec "Parser Lisp Basic" spec

spec :: Spec
spec = do
  it "parses a simple symbol" $ do
    parseProgram "foo" `shouldBe` Right [ESymbol "foo"]

  it "parses list with spaces" $ do
    parseProgram "(add 1 2)" `shouldBe` Right [EList [ESymbol "add", EInt 1, EInt 2]]
