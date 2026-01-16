module Parser.ParserCoverage (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Parser
import qualified AST

-- This module contains comprehensive tests for Parser.hs
-- Goal: Achieve maximum coverage of lexer and parser code paths

tests :: IO TestTree
tests = return $ testGroup "Parser Coverage"
  [ literalTests
  , operatorTests
  , functionTests
  , controlFlowTests
  , listTupleTests
  , blockTests
  , importTests
  , errorTests
  , desugarTests
  , edgeCaseTests
  , whitespaceTests
  , complexExpressionTests
  , moreErrorTests
  , returnTests
  , incrementDecrementTests
  , additionalOperatorTests
  , nestedStructuresTests
  ]

literalTests :: TestTree
literalTests = testGroup "Literal Parsing"
  [ testCase "Parse positive integer" $
      assertParsesAs "42" [AST.TLExpr (AST.EInt 42)]
  , testCase "Parse negative integer" $
      assertParsesAs "-42" [AST.TLExpr (AST.EUnary "-" (AST.EInt 42))]
  , testCase "Parse zero" $
      assertParsesAs "0" [AST.TLExpr (AST.EInt 0)]
  , testCase "Parse large integer" $
      assertParsesAs "9223372036854775807" [AST.TLExpr (AST.EInt 9223372036854775807)]
  , testCase "Parse true" $
      assertParsesAs "true" [AST.TLExpr (AST.EBool True)]
  , testCase "Parse false" $
      assertParsesAs "false" [AST.TLExpr (AST.EBool False)]
  , testCase "Parse simple string" $
      assertParsesAs "\"hello\"" [AST.TLExpr (AST.EString "hello")]
  , testCase "Parse empty string" $
      assertParsesAs "\"\"" [AST.TLExpr (AST.EString "")]
  , testCase "Parse string with spaces" $
      assertParsesAs "\"hello world\"" [AST.TLExpr (AST.EString "hello world")]
  , testCase "Parse string with escape sequences" $
      assertParsesAs "\"hello\\nworld\"" [AST.TLExpr (AST.EString "hello\nworld")]
  , testCase "Parse string with tab" $
      assertParsesAs "\"hello\\tworld\"" [AST.TLExpr (AST.EString "hello\tworld")]
  ]

operatorTests :: TestTree
operatorTests = testGroup "Operator Parsing"
  [ testCase "Parse addition" $
      assertParsesAs "1 + 2" [AST.TLExpr (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))]
  , testCase "Parse subtraction" $
      assertParsesAs "5 - 3" [AST.TLExpr (AST.EBinary AST.Sub (AST.EInt 5) (AST.EInt 3))]
  , testCase "Parse multiplication" $
      assertParsesAs "4 * 5" [AST.TLExpr (AST.EBinary AST.Mul (AST.EInt 4) (AST.EInt 5))]
  , testCase "Parse division" $
      assertParsesAs "10 / 2" [AST.TLExpr (AST.EBinary AST.Div (AST.EInt 10) (AST.EInt 2))]
  , testCase "Parse modulo" $
      assertParsesAs "10 % 3" [AST.TLExpr (AST.EBinary AST.Mod (AST.EInt 10) (AST.EInt 3))]
  , testCase "Parse equality" $
      assertParsesAs "5 == 5" [AST.TLExpr (AST.EBinary AST.Eq (AST.EInt 5) (AST.EInt 5))]
  , testCase "Parse inequality" $
      assertParsesAs "5 != 3" [AST.TLExpr (AST.EBinary AST.Neq (AST.EInt 5) (AST.EInt 3))]
  , testCase "Parse less than" $
      assertParsesAs "3 < 5" [AST.TLExpr (AST.EBinary AST.Lt (AST.EInt 3) (AST.EInt 5))]
  , testCase "Parse less than or equal" $
      assertParsesAs "3 <= 5" [AST.TLExpr (AST.EBinary AST.Lte (AST.EInt 3) (AST.EInt 5))]
  , testCase "Parse greater than" $
      assertParsesAs "5 > 3" [AST.TLExpr (AST.EBinary AST.Gt (AST.EInt 5) (AST.EInt 3))]
  , testCase "Parse greater than or equal" $
      assertParsesAs "5 >= 3" [AST.TLExpr (AST.EBinary AST.Gte (AST.EInt 5) (AST.EInt 3))]
  , testCase "Parse logical AND" $
      assertParsesAs "true && false" [AST.TLExpr (AST.EBinary AST.And (AST.EBool True) (AST.EBool False))]
  , testCase "Parse logical OR" $
      assertParsesAs "true || false" [AST.TLExpr (AST.EBinary AST.Or (AST.EBool True) (AST.EBool False))]
  , testCase "Parse unary minus" $
      assertParsesAs "-5" [AST.TLExpr (AST.EUnary "-" (AST.EInt 5))]
  , testCase "Parse unary NOT" $
      assertParsesAs "!true" [AST.TLExpr (AST.EUnary "!" (AST.EBool True))]
  , testCase "Parse operator precedence (mul before add)" $
      assertParsesAs "2 + 3 * 4" 
        [AST.TLExpr (AST.EBinary AST.Add (AST.EInt 2) (AST.EBinary AST.Mul (AST.EInt 3) (AST.EInt 4)))]
  , testCase "Parse operator precedence (parentheses)" $
      assertParsesAs "(2 + 3) * 4" 
        [AST.TLExpr (AST.EBinary AST.Mul (AST.EBinary AST.Add (AST.EInt 2) (AST.EInt 3)) (AST.EInt 4))]
  , testCase "Parse pipe operator" $
      assertParsesAs "5 |> f" [AST.TLExpr (AST.EBinary AST.Pipe (AST.EInt 5) (AST.EVar "f"))]
  ]

functionTests :: TestTree
functionTests = testGroup "Function Parsing"
  [ testCase "Parse simple function definition" $
      assertParsesAs "fn f(x) = x" [AST.TLFn "f" ["x"] (AST.EVar "x")]
  , testCase "Parse function with multiple parameters" $
      assertParsesAs "fn add(a, b) = a + b" 
        [AST.TLFn "add" ["a", "b"] (AST.EBinary AST.Add (AST.EVar "a") (AST.EVar "b"))]
  , testCase "Parse function with no parameters" $
      assertParsesAs "fn f() = 42" [AST.TLFn "f" [] (AST.EInt 42)]
  , testCase "Parse function with block body" $
      assertParsesAs "fn f(x) = { x }" 
        [AST.TLFn "f" ["x"] (AST.EBlock [] (Just (AST.EVar "x")))]
  , testCase "Parse procedure (no equals sign)" $
      assertParsesAs "fn proc(x) { let y = x }" 
        [AST.TLProc "proc" ["x"] [AST.TLLet "y" (AST.EVar "x")]]
  , testCase "Parse function call" $
      assertParsesAs "f(42)" [AST.TLExpr (AST.ECall (AST.EVar "f") [AST.EInt 42])]
  , testCase "Parse function call with multiple args" $
      assertParsesAs "add(1, 2)" [AST.TLExpr (AST.ECall (AST.EVar "add") [AST.EInt 1, AST.EInt 2])]
  , testCase "Parse function call with no args" $
      assertParsesAs "f()" [AST.TLExpr (AST.ECall (AST.EVar "f") [])]
  , testCase "Parse nested function calls" $
      assertParsesAs "f(g(x))" 
        [AST.TLExpr (AST.ECall (AST.EVar "f") [AST.ECall (AST.EVar "g") [AST.EVar "x"]])]
  , testCase "Parse lambda expression" $
      assertParsesAs "(x) => x + 1" 
        [AST.TLExpr (AST.ELam ["x"] (AST.EBinary AST.Add (AST.EVar "x") (AST.EInt 1)))]
  , testCase "Parse lambda with multiple params" $
      assertParsesAs "(a, b) => a + b" 
        [AST.TLExpr (AST.ELam ["a", "b"] (AST.EBinary AST.Add (AST.EVar "a") (AST.EVar "b")))]
  , testCase "Parse lambda with no params" $
      assertParsesAs "() => 42" [AST.TLExpr (AST.ELam [] (AST.EInt 42))]
  ]

controlFlowTests :: TestTree
controlFlowTests = testGroup "Control Flow Parsing"
  [ testCase "Parse simple if-else" $
      assertParsesAs "if true { 1 } else { 0 }" 
        [AST.TLExpr (AST.EIf (AST.EBool True) (AST.EInt 1) (AST.EInt 0))]
  , testCase "Parse if-else with comparison" $
      assertParsesAs "if x > 0 { 1 } else { 0 }" 
        [AST.TLExpr (AST.EIf (AST.EBinary AST.Gt (AST.EVar "x") (AST.EInt 0)) (AST.EInt 1) (AST.EInt 0))]
  , testCase "Parse if with block then branch" $
      assertParses "if true { x } else { y }"
  , testCase "Parse if with expression sequence in then" $
      assertParses "if true { 1 2 } else { 0 }"
  , testCase "Parse nested if-else" $
      assertParses "if x > 0 { if y > 0 { 1 } else { 2 } } else { 3 }"
  , testCase "Parse return statement" $
      assertParsesAs "RET 42" [AST.TLExpr (AST.ERet (AST.EInt 42))]
  ]

listTupleTests :: TestTree
listTupleTests = testGroup "List and Tuple Parsing"
  [ testCase "Parse empty list" $
      assertParsesAs "[]" [AST.TLExpr (AST.EList [])]
  , testCase "Parse list with one element" $
      assertParsesAs "[1]" [AST.TLExpr (AST.EList [AST.EInt 1])]
  , testCase "Parse list with multiple elements" $
      assertParsesAs "[1, 2, 3]" [AST.TLExpr (AST.EList [AST.EInt 1, AST.EInt 2, AST.EInt 3])]
  , testCase "Parse nested list" $
      assertParsesAs "[[1, 2], [3, 4]]" 
        [AST.TLExpr (AST.EList [AST.EList [AST.EInt 1, AST.EInt 2], AST.EList [AST.EInt 3, AST.EInt 4]])]
  , testCase "Parse tuple" $
      assertParsesAs "(1, 2)" [AST.TLExpr (AST.ETuple [AST.EInt 1, AST.EInt 2])]
  , testCase "Parse tuple with three elements" $
      assertParsesAs "(1, 2, 3)" [AST.TLExpr (AST.ETuple [AST.EInt 1, AST.EInt 2, AST.EInt 3])]
  , testCase "Parse tuple with mixed types" $
      assertParses "(1, \"hello\", true)"
  ]

blockTests :: TestTree
blockTests = testGroup "Block Parsing"
  [ testCase "Parse function with empty block" $
      assertParses "fn f() = { }"
  , testCase "Parse function with block expression" $
      assertParses "fn f() = { 42 }"
  , testCase "Parse function with block let and expression" $
      assertParses "fn f() = { let x = 5 x }"
  , testCase "Parse function with block function definition" $
      assertParses "fn outer() = { fn f(x) = x 42 }"
  , testCase "Parse let statement" $
      assertParsesAs "let x = 42" [AST.TLLet "x" (AST.EInt 42)]
  , testCase "Parse let with expression" $
      assertParsesAs "let x = 1 + 2" [AST.TLLet "x" (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))]
  , testCase "Parse let with += operator" $
      assertParsesAs "let x += 5" [AST.TLLet "x" (AST.EBinary AST.Add (AST.EVar "x") (AST.EInt 5))]
  , testCase "Parse let with -= operator" $
      assertParsesAs "let x -= 3" [AST.TLLet "x" (AST.EBinary AST.Sub (AST.EVar "x") (AST.EInt 3))]
  , testCase "Parse let with *= operator" $
      assertParsesAs "let x *= 2" [AST.TLLet "x" (AST.EBinary AST.Mul (AST.EVar "x") (AST.EInt 2))]
  , testCase "Parse let with /= operator" $
      assertParsesAs "let x /= 2" [AST.TLLet "x" (AST.EBinary AST.Div (AST.EVar "x") (AST.EInt 2))]
  , testCase "Parse let with %= operator" $
      assertParsesAs "let x %= 3" [AST.TLLet "x" (AST.EBinary AST.Mod (AST.EVar "x") (AST.EInt 3))]
  ]

importTests :: TestTree
importTests = testGroup "Import Parsing"
  [ testCase "Parse simple import" $
      assertParsesAs "import { f } from \"file.flux\"" [AST.TLImport "file.flux" ["f"]]
  , testCase "Parse import with multiple items" $
      assertParsesAs "import { f, g, h } from \"lib.flux\"" [AST.TLImport "lib.flux" ["f", "g", "h"]]
  , testCase "Parse import with path" $
      assertParsesAs "import { x } from \"./subdir/file.flux\"" [AST.TLImport "./subdir/file.flux" ["x"]]
  ]

errorTests :: TestTree
errorTests = testGroup "Error Cases"
  [ testCase "Invalid syntax fails to parse" $
      assertFails "let = 5"
  , testCase "Unclosed string fails" $
      assertFails "\"hello"
  , testCase "Unclosed parentheses fails" $
      assertFails "(1 + 2"
  , testCase "Unclosed list fails" $
      assertFails "[1, 2"
  , testCase "Invalid operator fails" $
      assertFails "1 ++"
  , testCase "Missing else fails" $
      assertFails "if true { 1 }"
  ]

desugarTests :: TestTree
desugarTests = testGroup "Desugaring Tests"
  [ testCase "Desugar simple pipe" $ do
      let expr = AST.EBinary AST.Pipe (AST.EInt 5) (AST.EVar "f")
      let result = Parser.desugarPipes expr
      result @?= AST.ECall (AST.EVar "f") [AST.EInt 5]
  , testCase "Desugar pipe with function call" $ do
      let expr = AST.EBinary AST.Pipe (AST.EInt 5) (AST.ECall (AST.EVar "f") [AST.EInt 10])
      let result = Parser.desugarPipes expr
      result @?= AST.ECall (AST.EVar "f") [AST.EInt 5, AST.EInt 10]
  , testCase "Desugar tuple pipe" $ do
      let expr = AST.EBinary AST.Pipe (AST.ETuple [AST.EInt 1, AST.EInt 2]) (AST.EVar "f")
      let result = Parser.desugarPipes expr
      result @?= AST.ECall (AST.EVar "f") [AST.EInt 1, AST.EInt 2]
  , testCase "Desugar nested expressions" $ do
      let expr = AST.EBinary AST.Add (AST.EInt 1) (AST.EBinary AST.Mul (AST.EInt 2) (AST.EInt 3))
      let result = Parser.desugarPipes expr
      result @?= expr  -- Should remain unchanged
  ]

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Cases"
  [ testCase "Parse variable name with underscore" $
      assertParsesAs "my_var" [AST.TLExpr (AST.EVar "my_var")]
  , testCase "Parse variable name with numbers" $
      assertParsesAs "var123" [AST.TLExpr (AST.EVar "var123")]
  , testCase "Parse with line comments" $
      assertParsesAs "// comment\n42" [AST.TLExpr (AST.EInt 42)]
  , testCase "Parse with multiple spaces" $
      assertParsesAs "1    +    2" [AST.TLExpr (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))]
  , testCase "Parse with tabs" $
      assertParsesAs "1\t+\t2" [AST.TLExpr (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))]
  , testCase "Parse with newlines" $
      assertParsesAs "1\n+\n2" [AST.TLExpr (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))]
  , testCase "Parse multiple top-level definitions" $
      assertParses "let x = 5\nlet y = 10\nfn f(a) = a"
  , testCase "Parse increment ++" $
      assertParses "x++"
  , testCase "Parse decrement --" $
      assertParses "x--"
  , testCase "Parse very long identifier" $
      assertParses "very_long_variable_name_with_many_characters_123"
  , testCase "Parse nested parentheses" $
      assertParsesAs "((((5))))" [AST.TLExpr (AST.EInt 5)]
  , testCase "Parse empty function params" $
      assertParses "fn f() = 42"
  , testCase "Parse function single param" $
      assertParses "fn f(x) = x"
  , testCase "Parse function many params" $
      assertParses "fn f(a, b, c, d, e, f, g) = a"
  ]

whitespaceTests :: TestTree
whitespaceTests = testGroup "Whitespace Handling"
  [ testCase "No spaces around operators" $
      assertParsesAs "1+2" [AST.TLExpr (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))]
  , testCase "Extra spaces everywhere" $
      assertParsesAs "  1  +  2  " [AST.TLExpr (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))]
  , testCase "Newline before operator" $
      assertParsesAs "1\n+ 2" [AST.TLExpr (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))]
  , testCase "Multiple newlines" $
      assertParsesAs "1\n\n\n+ 2" [AST.TLExpr (AST.EBinary AST.Add (AST.EInt 1) (AST.EInt 2))]
  , testCase "Spaces in function call" $
      assertParses "f  (  1  ,  2  )"
  , testCase "Spaces in list" $
      assertParsesAs "[  1  ,  2  ,  3  ]" [AST.TLExpr (AST.EList [AST.EInt 1, AST.EInt 2, AST.EInt 3])]
  , testCase "Spaces in tuple" $
      assertParsesAs "(  1  ,  2  )" [AST.TLExpr (AST.ETuple [AST.EInt 1, AST.EInt 2])]
  , testCase "Comment at end of line" $
      assertParsesAs "42 // this is a comment" [AST.TLExpr (AST.EInt 42)]
  , testCase "Comment between expressions" $
      assertParses "let x = 5 // comment\nlet y = 10"
  , testCase "Multiple comments" $
      assertParses "// first\n// second\n42"
  ]

complexExpressionTests :: TestTree
complexExpressionTests = testGroup "Complex Expression Parsing"
  [ testCase "Deeply nested arithmetic" $
      assertParses "1 + (2 * (3 - (4 / (5 % 2))))"
  , testCase "All operators mixed" $
      assertParses "1 + 2 * 3 - 4 / 5 % 2"
  , testCase "Comparison in arithmetic" $
      assertParses "(1 + 2) > (3 - 1)"
  , testCase "Boolean in if" $
      assertParses "if true { 1 } else { 0 }"
  , testCase "Nested function calls in expression" $
      assertParses "f(g(h(x)))"
  , testCase "Lambda as argument" $
      assertParses "map((x) => x + 1, [1, 2, 3])"
  , testCase "List of functions" $
      assertParses "[(x) => x + 1, (x) => x * 2]"
  , testCase "Tuple of lists" $
      assertParses "([1, 2], [3, 4])"
  , testCase "List of tuples" $
      assertParses "[(1, 2), (3, 4)]"
  , testCase "Nested if-else" $
      assertParses "if x { if y { 1 } else { 2 } } else { if z { 3 } else { 4 } }"
  , testCase "Function returning lambda" $
      assertParses "fn makeAdder(x) = (y) => x + y"
  , testCase "Chained comparisons" $
      assertParses "a < b && b < c && c < d"
  , testCase "Mixed AND OR" $
      assertParses "a && b || c && d"
  , testCase "NOT with comparison" $
      assertParses "!(x > 5)"
  , testCase "Multiple unary operators" $
      assertParses "--5"
  , testCase "Unary on expression" $
      assertParses "-(5 + 3)"
  , testCase "Call on lambda" $
      assertParses "((x) => x + 1)(5)"
  , testCase "Pipeline simple" $
      assertParses "5 |> f"
  , testCase "Pipeline chain" $
      assertParses "5 |> f |> g |> h"
  , testCase "Pipeline with call" $
      assertParses "5 |> f(10)"
  ]

moreErrorTests :: TestTree
moreErrorTests = testGroup "Additional Error Cases"
  [ testCase "Unclosed string" $ assertParseFails "\"hello"
  , testCase "Unclosed parenthesis" $ assertParseFails "(1 + 2"
  , testCase "Unclosed bracket" $ assertParseFails "[1, 2"
  , testCase "Unclosed brace" $ assertParseFails "{ 1 + 2"
  , testCase "Missing function body" $ assertParseFails "fn f(x) ="
  , testCase "Missing condition" $ assertParseFails "if { 1 }"
  , testCase "Missing else after if" $ assertParseFails "if true { 1 } else"
  , testCase "Invalid identifier starts with digit" $ assertParseFails "let 1x = 5"
  , testCase "Missing equals in let" $ assertParseFails "let x 5"
  , testCase "Empty function parameters" $ assertParseFails "fn f() ="
  , testCase "Mismatched parens" $ assertParseFails "(1 + 2]"
  , testCase "Missing operand" $ assertParseFails "1 +"
  , testCase "Invalid escape sequence" $ assertParseFails "\"\\x\""
  ]

returnTests :: TestTree
returnTests = testGroup "Return Statement Parsing"
  [ testCase "Simple return" $
      assertParses "fn f() { RET 5 }"
  , testCase "Return with expression" $
      assertParses "fn f() { RET 1 + 2 }"
  , testCase "Return with function call" $
      assertParses "fn f() { RET g(10) }"
  , testCase "Return boolean" $
      assertParses "fn f() { RET true }"
  , testCase "Return string" $
      assertParses "fn f() { RET \"hello\" }"
  , testCase "Return list" $
      assertParses "fn f() { RET [1, 2, 3] }"
  , testCase "Early return" $
      assertParses "fn f(x) = if x > 0 { RET 1 } else { 2 }"
  ]

incrementDecrementTests :: TestTree
incrementDecrementTests = testGroup "Increment/Decrement Parsing"
  [ testCase "Post-increment" $
      assertParses "x++"
  , testCase "Post-decrement" $
      assertParses "x--"
  , testCase "Increment in expression" $
      assertParses "x++ + 5"
  , testCase "Decrement in expression" $
      assertParses "x-- + 5"
  , testCase "Multiple increments" $
      assertParses "x++\ny++"
  , testCase "Increment in function" $
      assertParses "fn f() { x++ }"
  ]

additionalOperatorTests :: TestTree
additionalOperatorTests = testGroup "Additional Operator Tests"
  [ testCase "Greater than or equal" $
      assertParses "5 >= 3"
  , testCase "Less than or equal" $
      assertParses "3 <= 5"
  , testCase "Not equal" $
      assertParses "5 != 3"
  , testCase "Complex comparison chain" $
      assertParses "1 < 2 && 2 < 3 && 3 < 4"
  , testCase "Mixed boolean operators" $
      assertParses "true && false || true"
  , testCase "Boolean with comparison" $
      assertParses "(5 > 3) && true"
  , testCase "Negation of comparison" $
      assertParses "!(5 > 3)"
  , testCase "Double negation" $
      assertParses "!(!true)"
  , testCase "Unary minus with parens" $
      assertParses "-(5 + 3)"
  , testCase "Multiple unary operators" $
      assertParses "!true"
  ]

nestedStructuresTests :: TestTree
nestedStructuresTests = testGroup "Deeply Nested Structures"
  [ testCase "Deeply nested lists" $
      assertParses "[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]"
  , testCase "Deeply nested tuples" $
      assertParses "((1, 2), (3, (4, 5)))"
  , testCase "Nested function calls" $
      assertParses "f(g(h(i(j(5)))))"
  , testCase "Deeply nested if" $
      assertParses "if true { if false { if true { 1 } else { 2 } } else { 3 } } else { 4 }"
  , testCase "Nested blocks" $
      assertParses "fn f() = { fn g() = { fn h() = 1 } }"
  , testCase "Mixed nested structures" $
      assertParses "f([1, (2, 3), g(4)])"
  , testCase "Complex nested expression" $
      assertParses "((1 + 2) * (3 - 4)) / ((5 + 6) % 7)"
  , testCase "Nested lambda in list" $
      assertParses "[(x) => x + 1, (y) => y * 2]"
  , testCase "Nested function definitions" $
      assertParses "fn outer() { fn inner() { fn innermost() = 5 } }"
  , testCase "Pipeline in function" $
      assertParses "fn f(x) = x |> (y) => y + 1"
  ]

-- Helper functions
assertParsesAs :: String -> AST.Program -> Assertion
assertParsesAs input expected =
  case Parser.parseProgram input of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right ast -> ast @?= expected

assertParses :: String -> Assertion
assertParses input =
  case Parser.parseProgram input of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right _ -> return ()

assertParseFails :: String -> Assertion
assertParseFails input =
  case Parser.parseProgram input of
    Left _ -> return ()
    Right _ -> assertFailure "Expected parse error but succeeded"

assertFails :: String -> Assertion
assertFails input =
  case Parser.parseProgram input of
    Left _ -> return ()
    Right _ -> assertFailure "Expected parse error but succeeded"
