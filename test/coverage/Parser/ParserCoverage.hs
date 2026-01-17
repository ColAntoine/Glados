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
  , exhaustiveLiteralTests
  , exhaustiveOperatorTests
  , exhaustiveFunctionTests
  , exhaustiveControlFlowTests
  , exhaustiveListTests
  , exhaustiveStringTests
  , exhaustiveVariableTests
  , comprehensiveMixedTests
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

exhaustiveLiteralTests :: TestTree
exhaustiveLiteralTests = testGroup "Exhaustive Literal Tests"
  [ testCase "Int 1" $ assertParses "1"
  , testCase "Int 100" $ assertParses "100"
  , testCase "Int 999999" $ assertParses "999999"
  , testCase "Int max" $ assertParses "9223372036854775807"
  , testCase "Negative int" $ assertParses "-123"
  , testCase "Negative max" $ assertParses "-9223372036854775807"
  , testCase "Bool true 1" $ assertParses "true"
  , testCase "Bool false 1" $ assertParses "false"
  , testCase "String abc" $ assertParses "\"abc\""
  , testCase "String xyz" $ assertParses "\"xyz\""
  , testCase "String with numbers" $ assertParses "\"test123\""
  , testCase "String with special chars" $ assertParses "\"hello-world_test\""
  , testCase "Empty string 2" $ assertParses "\"\""
  , testCase "String newline escape" $ assertParses "\"line1\\nline2\""
  , testCase "String tab escape" $ assertParses "\"col1\\tcol2\""
  , testCase "String quote escape" $ assertParses "\"say \\\"hello\\\"\""
  , testCase "String backslash escape" $ assertParses "\"path\\\\to\\\\file\""
  , testCase "Long string" $ assertParses "\"this is a very long string with many words in it\""
  , testCase "String with punctuation" $ assertParses "\"Hello, World! How are you?\""
  , testCase "String with digits" $ assertParses "\"12345\""
  ]

exhaustiveOperatorTests :: TestTree
exhaustiveOperatorTests = testGroup "Exhaustive Operator Tests"
  [ testCase "Add chain" $ assertParses "1 + 2 + 3 + 4"
  , testCase "Sub chain" $ assertParses "10 - 1 - 2 - 3"
  , testCase "Mul chain" $ assertParses "2 * 3 * 4"
  , testCase "Div chain" $ assertParses "100 / 2 / 5"
  , testCase "Mod chain" $ assertParses "100 % 7 % 3"
  , testCase "Mixed arithmetic 1" $ assertParses "1 + 2 * 3"
  , testCase "Mixed arithmetic 2" $ assertParses "10 - 4 / 2"
  , testCase "Mixed arithmetic 3" $ assertParses "5 * 3 + 2"
  , testCase "Mixed arithmetic 4" $ assertParses "20 / 4 - 1"
  , testCase "Mixed arithmetic 5" $ assertParses "7 % 3 + 1"
  , testCase "Comparison eq" $ assertParses "10 == 10"
  , testCase "Comparison neq" $ assertParses "5 != 3"
  , testCase "Comparison lt" $ assertParses "2 < 5"
  , testCase "Comparison lte" $ assertParses "3 <= 3"
  , testCase "Comparison gt" $ assertParses "7 > 2"
  , testCase "Comparison gte" $ assertParses "5 >= 5"
  , testCase "Bool and 1" $ assertParses "true && true"
  , testCase "Bool and 2" $ assertParses "false && true"
  , testCase "Bool or 1" $ assertParses "true || false"
  , testCase "Bool or 2" $ assertParses "false || false"
  , testCase "Unary minus 1" $ assertParses "-10"
  , testCase "Unary minus 2" $ assertParses "-999"
  , testCase "Unary not 1" $ assertParses "!false"
  , testCase "Unary not 2" $ assertParses "!true"
  , testCase "Comparison with add" $ assertParses "(1 + 2) == 3"
  , testCase "Comparison with sub" $ assertParses "(10 - 5) > 3"
  , testCase "Comparison with mul" $ assertParses "(2 * 3) < 10"
  , testCase "Bool with comparison 1" $ assertParses "(5 > 3) && (2 < 4)"
  , testCase "Bool with comparison 2" $ assertParses "(10 == 10) || (5 != 5)"
  , testCase "Complex precedence 1" $ assertParses "1 + 2 * 3 - 4"
  , testCase "Complex precedence 2" $ assertParses "10 / 2 + 3 * 4"
  , testCase "Complex precedence 3" $ assertParses "5 * 2 - 8 / 4"
  , testCase "Parentheses priority 1" $ assertParses "(1 + 2) * 3"
  , testCase "Parentheses priority 2" $ assertParses "5 * (2 + 3)"
  , testCase "Parentheses priority 3" $ assertParses "(10 - 2) / 4"
  , testCase "Nested parens 1" $ assertParses "((1 + 2))"
  , testCase "Nested parens 2" $ assertParses "(((5)))"
  , testCase "Nested parens 3" $ assertParses "((1 + 2) * (3 + 4))"
  ]

exhaustiveFunctionTests :: TestTree
exhaustiveFunctionTests = testGroup "Exhaustive Function Tests"
  [ testCase "Fn no params return int" $ assertParses "fn f() = 42"
  , testCase "Fn one param return int" $ assertParses "fn f(x) = x"
  , testCase "Fn two params add" $ assertParses "fn f(x, y) = x + y"
  , testCase "Fn three params" $ assertParses "fn f(a, b, c) = a + b + c"
  , testCase "Fn four params" $ assertParses "fn f(a, b, c, d) = a + b + c + d"
  , testCase "Fn five params" $ assertParses "fn f(a, b, c, d, e) = a + b + c + d + e"
  , testCase "Fn return bool" $ assertParses "fn f() = true"
  , testCase "Fn return string" $ assertParses "fn f() = \"hello\""
  , testCase "Fn return list" $ assertParses "fn f() = [1, 2, 3]"
  , testCase "Fn return tuple" $ assertParses "fn f() = (1, 2)"
  , testCase "Fn with if" $ assertParses "fn f(x) = if x > 0 { 1 } else { 0 }"
  , testCase "Fn with nested if" $ assertParses "fn f(x) = if x > 10 { if x > 20 { 2 } else { 1 } } else { 0 }"
  , testCase "Fn call no args" $ assertParses "f()"
  , testCase "Fn call one arg" $ assertParses "f(42)"
  , testCase "Fn call two args" $ assertParses "f(1, 2)"
  , testCase "Fn call three args" $ assertParses "f(1, 2, 3)"
  , testCase "Fn call nested" $ assertParses "f(g(h(5)))"
  , testCase "Fn call chain" $ assertParses "f(1)(2)(3)"
  , testCase "Lambda no params" $ assertParses "() => 42"
  , testCase "Lambda one param" $ assertParses "(x) => x + 1"
  , testCase "Lambda two params" $ assertParses "(x, y) => x + y"
  , testCase "Lambda three params" $ assertParses "(a, b, c) => a + b + c"
  , testCase "Lambda return bool" $ assertParses "(x) => x > 0"
  , testCase "Lambda return string" $ assertParses "() => \"test\""
  , testCase "Lambda in list" $ assertParses "[(x) => x + 1]"
  , testCase "Lambda in tuple" $ assertParses "((x) => x, (y) => y)"
  , testCase "Lambda call" $ assertParses "((x) => x + 1)(5)"
  ]

exhaustiveControlFlowTests :: TestTree
exhaustiveControlFlowTests = testGroup "Exhaustive Control Flow Tests"
  [ testCase "If true then int" $ assertParses "if true { 1 } else { 0 }"
  , testCase "If false then int" $ assertParses "if false { 0 } else { 1 }"
  , testCase "If comparison then" $ assertParses "if 5 > 3 { 1 } else { 0 }"
  , testCase "If equality then" $ assertParses "if 5 == 5 { 1 } else { 0 }"
  , testCase "If bool and then" $ assertParses "if true && false { 1 } else { 0 }"
  , testCase "If bool or then" $ assertParses "if true || false { 1 } else { 0 }"
  , testCase "If nested 1" $ assertParses "if true { if true { 1 } else { 2 } } else { 3 }"
  , testCase "If nested 2" $ assertParses "if false { 1 } else { if true { 2 } else { 3 } }"
  , testCase "If nested 3" $ assertParses "if true { if false { 1 } else { if true { 2 } else { 3 } } } else { 4 }"
  , testCase "If then multiple stmts" $ assertParses "if true { 1 2 3 } else { 0 }"
  , testCase "If else multiple stmts" $ assertParses "if false { 0 } else { 1 2 3 }"
  , testCase "If with function call" $ assertParses "if f(5) { 1 } else { 0 }"
  , testCase "If with arithmetic" $ assertParses "if 1 + 1 == 2 { 1 } else { 0 }"
  , testCase "If return int 1" $ assertParses "if true { 42 } else { 0 }"
  , testCase "If return int 2" $ assertParses "if false { 0 } else { 99 }"
  , testCase "If return bool" $ assertParses "if true { true } else { false }"
  , testCase "If return string" $ assertParses "if true { \"yes\" } else { \"no\" }"
  , testCase "If return list" $ assertParses "if true { [1, 2] } else { [3, 4] }"
  ]

exhaustiveListTests :: TestTree
exhaustiveListTests = testGroup "Exhaustive List Tests"
  [ testCase "Empty list 1" $ assertParses "[]"
  , testCase "List one int" $ assertParses "[1]"
  , testCase "List two ints" $ assertParses "[1, 2]"
  , testCase "List three ints" $ assertParses "[1, 2, 3]"
  , testCase "List five ints" $ assertParses "[1, 2, 3, 4, 5]"
  , testCase "List ten ints" $ assertParses "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
  , testCase "List of bools" $ assertParses "[true, false, true]"
  , testCase "List of strings" $ assertParses "[\"a\", \"b\", \"c\"]"
  , testCase "List mixed types" $ assertParses "[1, true, \"test\"]"
  , testCase "List of lists ints" $ assertParses "[[1, 2], [3, 4]]"
  , testCase "List of lists nested" $ assertParses "[[[1]], [[2]]]"
  , testCase "List of tuples" $ assertParses "[(1, 2), (3, 4)]"
  , testCase "List with arithmetic" $ assertParses "[1 + 1, 2 + 2]"
  , testCase "List with comparison" $ assertParses "[1 < 2, 3 > 4]"
  , testCase "List with function calls" $ assertParses "[f(1), g(2)]"
  , testCase "Tuple empty elements" $ assertParses "(1, 2)"
  , testCase "Tuple three elements" $ assertParses "(1, 2, 3)"
  , testCase "Tuple four elements" $ assertParses "(1, 2, 3, 4)"
  , testCase "Tuple five elements" $ assertParses "(1, 2, 3, 4, 5)"
  , testCase "Tuple with strings" $ assertParses "(\"a\", \"b\", \"c\")"
  , testCase "Tuple with bools" $ assertParses "(true, false, true)"
  , testCase "Tuple mixed types" $ assertParses "(1, \"test\", true)"
  , testCase "Tuple with lists" $ assertParses "([1, 2], [3, 4])"
  , testCase "Tuple with tuples" $ assertParses "((1, 2), (3, 4))"
  , testCase "Nested tuple 3 levels" $ assertParses "(((1, 2), 3), 4)"
  ]

exhaustiveStringTests :: TestTree
exhaustiveStringTests = testGroup "Exhaustive String Tests"
  [ testCase "String a" $ assertParses "\"a\""
  , testCase "String hello" $ assertParses "\"hello\""
  , testCase "String world" $ assertParses "\"world\""
  , testCase "String test" $ assertParses "\"test\""
  , testCase "String foo" $ assertParses "\"foo\""
  , testCase "String bar" $ assertParses "\"bar\""
  , testCase "String with space 1" $ assertParses "\"hello world\""
  , testCase "String with space 2" $ assertParses "\"foo bar baz\""
  , testCase "String with newline" $ assertParses "\"line1\\nline2\""
  , testCase "String with tab" $ assertParses "\"col1\\tcol2\""
  , testCase "String with quote" $ assertParses "\"say \\\"hi\\\"\""
  , testCase "String with backslash" $ assertParses "\"path\\\\file\""
  , testCase "String with numbers 1" $ assertParses "\"test123\""
  , testCase "String with numbers 2" $ assertParses "\"abc456def\""
  , testCase "String sentence" $ assertParses "\"This is a sentence.\""
  , testCase "String question" $ assertParses "\"How are you?\""
  , testCase "String exclamation" $ assertParses "\"Hello!\""
  , testCase "String comma" $ assertParses "\"one, two, three\""
  , testCase "String period" $ assertParses "\"end.\""
  , testCase "String underscore" $ assertParses "\"hello_world\""
  ]

exhaustiveVariableTests :: TestTree
exhaustiveVariableTests = testGroup "Exhaustive Variable Tests"
  [ testCase "Let x int" $ assertParses "let x = 1"
  , testCase "Let y int" $ assertParses "let y = 42"
  , testCase "Let z bool" $ assertParses "let z = true"
  , testCase "Let a string" $ assertParses "let a = \"test\""
  , testCase "Let b list" $ assertParses "let b = [1, 2, 3]"
  , testCase "Let c tuple" $ assertParses "let c = (1, 2)"
  , testCase "Let with arithmetic" $ assertParses "let x = 1 + 2"
  , testCase "Let with comparison" $ assertParses "let b = 5 > 3"
  , testCase "Let with function call" $ assertParses "let result = f(10)"
  , testCase "Let with lambda" $ assertParses "let fn = (x) => x + 1"
  , testCase "Var x" $ assertParses "x"
  , testCase "Var abc" $ assertParses "abc"
  , testCase "Var test123" $ assertParses "test123"
  , testCase "Var with underscore" $ assertParses "var_name"
  , testCase "Var in arithmetic" $ assertParses "x + y"
  , testCase "Var in comparison" $ assertParses "x > y"
  , testCase "Var in function call" $ assertParses "f(x, y)"
  ]

comprehensiveMixedTests :: TestTree
comprehensiveMixedTests = testGroup "Comprehensive Mixed Tests"
  [ testCase "Program with pipeline" $ assertParses "5 |> (x) => x + 1"
  , testCase "Program with multiple pipelines" $ assertParses "5 |> (x) => x + 1 |> (y) => y * 2"
  , testCase "Program with import" $ assertParses "import { func } from \"module.flux\""
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
