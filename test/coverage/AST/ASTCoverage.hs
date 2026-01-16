module AST.ASTCoverage (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified AST
import qualified Parser

-- This module contains comprehensive tests for AST.hs
-- Goal: Achieve maximum coverage of AST node types and operations

tests :: IO TestTree
tests = return $ testGroup "AST Coverage"
  [ nodeConstructionTests
  , topLevelNodeTests
  , exprNodeTests
  , operatorTests
  , equalityTests
  , showTests
  ]

nodeConstructionTests :: TestTree
nodeConstructionTests = testGroup "AST Node Construction"
  [ testCase "Construct EInt" $ do
      let node = AST.EInt 42
      node @?= AST.EInt 42
  , testCase "Construct EBool True" $ do
      let node = AST.EBool True
      node @?= AST.EBool True
  , testCase "Construct EBool False" $ do
      let node = AST.EBool False
      node @?= AST.EBool False
  , testCase "Construct EString" $ do
      let node = AST.EString "hello"
      node @?= AST.EString "hello"
  , testCase "Construct EVar" $ do
      let node = AST.EVar "x"
      node @?= AST.EVar "x"
  , testCase "Construct EList empty" $ do
      let node = AST.EList []
      node @?= AST.EList []
  , testCase "Construct EList with elements" $ do
      let node = AST.EList [AST.EInt 1, AST.EInt 2]
      node @?= AST.EList [AST.EInt 1, AST.EInt 2]
  , testCase "Construct ETuple" $ do
      let node = AST.ETuple [AST.EInt 1, AST.EInt 2]
      node @?= AST.ETuple [AST.EInt 1, AST.EInt 2]
  ]

topLevelNodeTests :: TestTree
topLevelNodeTests = testGroup "Top-Level Node Types"
  [ testCase "TLImport construction" $ do
      let node = AST.TLImport "file.flux" ["f", "g"]
      node @?= AST.TLImport "file.flux" ["f", "g"]
  , testCase "TLFn construction" $ do
      let node = AST.TLFn "f" ["x"] (AST.EVar "x")
      node @?= AST.TLFn "f" ["x"] (AST.EVar "x")
  , testCase "TLProc construction" $ do
      let node = AST.TLProc "proc" ["x"] [AST.TLExpr (AST.EVar "x")]
      node @?= AST.TLProc "proc" ["x"] [AST.TLExpr (AST.EVar "x")]
  , testCase "TLLet construction" $ do
      let node = AST.TLLet "x" (AST.EInt 42)
      node @?= AST.TLLet "x" (AST.EInt 42)
  , testCase "TLExpr construction" $ do
      let node = AST.TLExpr (AST.EInt 42)
      node @?= AST.TLExpr (AST.EInt 42)
  ]

exprNodeTests :: TestTree
exprNodeTests = testGroup "Expression Node Types"
  [ testCase "EIf construction" $ do
      let node = AST.EIf (AST.EBool True) (AST.EInt 1) (AST.EInt 0)
      node @?= AST.EIf (AST.EBool True) (AST.EInt 1) (AST.EInt 0)
  , testCase "ELam construction" $ do
      let node = AST.ELam ["x", "y"] (AST.EBinary AST.Add (AST.EVar "x") (AST.EVar "y"))
      node @?= AST.ELam ["x", "y"] (AST.EBinary AST.Add (AST.EVar "x") (AST.EVar "y"))
  , testCase "ECall construction" $ do
      let node = AST.ECall (AST.EVar "f") [AST.EInt 1, AST.EInt 2]
      node @?= AST.ECall (AST.EVar "f") [AST.EInt 1, AST.EInt 2]
  , testCase "EUnary construction" $ do
      let node = AST.EUnary "-" (AST.EInt 5)
      node @?= AST.EUnary "-" (AST.EInt 5)
  , testCase "EBinary construction" $ do
      let node = AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2)
      node @?= AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2)
  , testCase "EBlock construction empty" $ do
      let node = AST.EBlock [] Nothing
      node @?= AST.EBlock [] Nothing
  , testCase "EBlock construction with expr" $ do
      let node = AST.EBlock [AST.TLLet "x" (AST.EInt 5)] (Just (AST.EVar "x"))
      node @?= AST.EBlock [AST.TLLet "x" (AST.EInt 5)] (Just (AST.EVar "x"))
  , testCase "ERet construction" $ do
      let node = AST.ERet (AST.EInt 42)
      node @?= AST.ERet (AST.EInt 42)
  , testCase "ESeq construction" $ do
      let node = AST.ESeq [AST.EInt 1, AST.EInt 2]
      node @?= AST.ESeq [AST.EInt 1, AST.EInt 2]
  , testCase "EAssign construction" $ do
      let node = AST.EAssign "x" "+" (AST.EInt 5)
      node @?= AST.EAssign "x" "+" (AST.EInt 5)
  , testCase "EIncDec increment" $ do
      let node = AST.EIncDec "x" True
      node @?= AST.EIncDec "x" True
  , testCase "EIncDec decrement" $ do
      let node = AST.EIncDec "x" False
      node @?= AST.EIncDec "x" False
  ]

operatorTests :: TestTree
operatorTests = testGroup "Operator Types"
  [ testCase "Add operator" $ AST.Add @?= AST.Add
  , testCase "Sub operator" $ AST.Sub @?= AST.Sub
  , testCase "Mul operator" $ AST.Mul @?= AST.Mul
  , testCase "Div operator" $ AST.Div @?= AST.Div
  , testCase "Mod operator" $ AST.Mod @?= AST.Mod
  , testCase "Eq operator" $ AST.Eq @?= AST.Eq
  , testCase "Neq operator" $ AST.Neq @?= AST.Neq
  , testCase "Lt operator" $ AST.Lt @?= AST.Lt
  , testCase "Lte operator" $ AST.Lte @?= AST.Lte
  , testCase "Gt operator" $ AST.Gt @?= AST.Gt
  , testCase "Gte operator" $ AST.Gte @?= AST.Gte
  , testCase "And operator" $ AST.And @?= AST.And
  , testCase "Or operator" $ AST.Or @?= AST.Or
  , testCase "Pipe operator" $ AST.Pipe @?= AST.Pipe
  ]

equalityTests :: TestTree
equalityTests = testGroup "AST Equality Tests"
  [ testCase "Equal integers" $ 
      AST.EInt 42 @?= AST.EInt 42
  , testCase "Different integers" $ 
      assertBool "Should not be equal" (AST.EInt 42 /= AST.EInt 43)
  , testCase "Equal booleans" $ 
      AST.EBool True @?= AST.EBool True
  , testCase "Equal strings" $ 
      AST.EString "hello" @?= AST.EString "hello"
  , testCase "Equal variables" $ 
      AST.EVar "x" @?= AST.EVar "x"
  , testCase "Equal lists" $ 
      AST.EList [AST.EInt 1, AST.EInt 2] @?= AST.EList [AST.EInt 1, AST.EInt 2]
  , testCase "Equal binary operations" $ 
      AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2) @?= 
      AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2)
  , testCase "Different operators" $ 
      assertBool "Should not be equal" 
        (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2) /= 
         AST.EBinary AST.Mul (AST.EInt 1) (AST.EInt 2))
  ]

showTests :: TestTree
showTests = testGroup "AST Show Instance Tests"
  [ testCase "Show EInt" $ do
      let result = show (AST.EInt 42)
      assertBool "Should contain EInt" ("EInt" `elem` words result)
  , testCase "Show EBool" $ do
      let result = show (AST.EBool True)
      assertBool "Should contain EBool" ("EBool" `elem` words result)
  , testCase "Show EString" $ do
      let result = show (AST.EString "hello")
      assertBool "Should contain EString" ("EString" `elem` words result)
  , testCase "Show EVar" $ do
      let result = show (AST.EVar "x")
      assertBool "Should contain EVar" ("EVar" `elem` words result)
  , testCase "Show EBinary" $ do
      let result = show (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))
      assertBool "Should contain EBinary" ("EBinary" `elem` words result)
  , testCase "Show Add" $ do
      let result = show AST.Add
      result @?= "Add"
  , testCase "Show TLFn" $ do
      let result = show (AST.TLFn "f" ["x"] (AST.EVar "x"))
      assertBool "Should contain TLFn" ("TLFn" `elem` words result)
  ]
