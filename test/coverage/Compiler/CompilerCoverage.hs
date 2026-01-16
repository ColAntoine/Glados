module Compiler.CompilerCoverage (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Parser
import qualified Compiler
import Data.List (isInfixOf)

-- This module contains comprehensive tests for Compiler.hs and its submodules
-- Goal: Achieve maximum coverage of compilation, codegen, boxing, and type handling

tests :: IO TestTree
tests = return $ testGroup "Compiler Coverage"
  [ basicCompilationTests
  , expressionTests
  , functionTests
  , controlFlowTests
  , dataStructureTests
  , codegenTests
  , advancedCompilationTests
  , errorHandlingTests
  , additionalExpressionTests
  , additionalFunctionTests
  , additionalControlFlowTests
  , boxingTests
  , typeTests
  ]

basicCompilationTests :: TestTree
basicCompilationTests = testGroup "Basic Compilation"
  [ testCase "Compile integer" $ compiles "42"
  , testCase "Compile boolean true" $ compiles "true"
  , testCase "Compile boolean false" $ compiles "false"
  , testCase "Compile string" $ compiles "\"hello\""
  , testCase "Compile empty string" $ compiles "\"\""
  , testCase "Compile variable reference" $ compiles "let x = 5\nx"
  , testCase "Compile let binding" $ compiles "let x = 42"
  , testCase "Compile multiple bindings" $ compiles "let x = 1\nlet y = 2"
  ]

expressionTests :: TestTree
expressionTests = testGroup "Expression Compilation"
  [ testCase "Compile addition" $ compiles "1 + 2"
  , testCase "Compile subtraction" $ compiles "5 - 3"
  , testCase "Compile multiplication" $ compiles "4 * 5"
  , testCase "Compile division" $ compiles "10 / 2"
  , testCase "Compile modulo" $ compiles "10 % 3"
  , testCase "Compile unary minus" $ compiles "-5"
  , testCase "Compile unary not" $ compiles "!true"
  , testCase "Compile equality" $ compiles "5 == 5"
  , testCase "Compile inequality" $ compiles "5 != 3"
  , testCase "Compile less than" $ compiles "3 < 5"
  , testCase "Compile less than or equal" $ compiles "3 <= 5"
  , testCase "Compile greater than" $ compiles "5 > 3"
  , testCase "Compile greater than or equal" $ compiles "5 >= 3"
  , testCase "Compile logical AND" $ compiles "true && false"
  , testCase "Compile logical OR" $ compiles "true || false"
  , testCase "Compile complex expression" $ compiles "(2 + 3) * (4 - 1)"
  ]

functionTests :: TestTree
functionTests = testGroup "Function Compilation"
  [ testCase "Compile simple function" $ compiles "fn f(x) = x"
  , testCase "Compile function with arithmetic" $ compiles "fn add(a, b) = a + b"
  , testCase "Compile function with multiple params" $ compiles "fn mul(a, b, c) = a * b * c"
  , testCase "Compile function with no params" $ compiles "fn const() = 42"
  , testCase "Compile function call" $ compiles "fn f(x) = x\nf(42)"
  , testCase "Compile recursive function" $ compiles "fn fact(n) = if n <= 1 { 1 } else { n * fact(n - 1) }"
  , testCase "Compile lambda" $ compiles "let f = (x) => x + 1"
  , testCase "Compile procedure" $ compiles "fn proc(x) { let y = x }"
  , testCase "Compile nested function calls" $ compiles "fn f(x) = x + 1\nfn g(x) = f(x)\ng(5)"
  ]

controlFlowTests :: TestTree
controlFlowTests = testGroup "Control Flow Compilation"
  [ testCase "Compile if-else" $ compiles "if true { 1 } else { 0 }"
  , testCase "Compile if with comparison" $ compiles "if 5 > 3 { 1 } else { 0 }"
  , testCase "Compile nested if" $ compiles "if true { if false { 1 } else { 2 } } else { 3 }"
  , testCase "Compile if with blocks" $ compiles "if true { let x = 5 x } else { 0 }"
  , testCase "Compile function with block" $ compiles "fn f() = { let x = 5 x + 1 }\nf()"
  , testCase "Compile function with empty block" $ compiles "fn f() = { }"
  , testCase "Compile return" $ compiles "RET 42"
  ]

dataStructureTests :: TestTree
dataStructureTests = testGroup "Data Structure Compilation"
  [ testCase "Compile empty list" $ compiles "[]"
  , testCase "Compile list with elements" $ compiles "[1, 2, 3]"
  , testCase "Compile nested list" $ compiles "[[1, 2], [3, 4]]"
  , testCase "Compile tuple" $ compiles "(1, 2, 3)"
  , testCase "Compile tuple with expressions" $ compiles "(1 + 1, 2 * 2)"
  ]

codegenTests :: TestTree
codegenTests = testGroup "Code Generation Tests"
  [ testCase "Generated code contains main function" $ do
      let code = compileToLLVM "42"
      assertBool "Should contain main function" ("define i32 @main()" `isInfixOf` code)
  , testCase "Generated code contains prelude" $ do
      let code = compileToLLVM "42"
      assertBool "Should contain type definitions" ("%Value = type" `isInfixOf` code)
  , testCase "Function definition generates LLVM function" $ do
      let code = compileToLLVM "fn f(x) = x"
      assertBool "Should contain function definition" ("define %Value @f(" `isInfixOf` code)
  , testCase "Integer literal in code" $ do
      let code = compileToLLVM "42"
      assertBool "Should contain i64" ("i64" `isInfixOf` code)
  , testCase "Arithmetic generates operations" $ do
      let code = compileToLLVM "1 + 2"
      assertBool "Should contain arithmetic" ("add" `isInfixOf` code || "call" `isInfixOf` code)
  , testCase "If-else generates branches" $ do
      let code = compileToLLVM "if true { 1 } else { 0 }"
      assertBool "Should contain branch" ("br " `isInfixOf` code)
  , testCase "Multiple functions in code" $ do
      let code = compileToLLVM "fn f(x) = x\nfn g(y) = y"
      assertBool "Should contain both functions" ("@f(" `isInfixOf` code && "@g(" `isInfixOf` code)
  , testCase "Let binding generates allocation" $ do
      let code = compileToLLVM "let x = 42"
      assertBool "Should contain alloca or store" ("alloca" `isInfixOf` code || "store" `isInfixOf` code)
  , testCase "Function call generates call instruction" $ do
      let code = compileToLLVM "fn f(x) = x\nf(42)"
      assertBool "Should contain call" ("call" `isInfixOf` code)
  ]

advancedCompilationTests :: TestTree
advancedCompilationTests = testGroup "Advanced Compilation Tests"
  [ testCase "Compile nested functions" $ compiles "fn outer(x) = { fn inner(y) = y + 1 inner(x) }"
  , testCase "Compile closure" $ compiles "fn makeAdder(x) = (y) => y + x"
  , testCase "Compile recursive calls" $ compiles "fn fact(n) = if n <= 1 { 1 } else { n * fact(n - 1) }\nfact(5)"
  , testCase "Compile lambda expressions" $ compiles "let f = (x) => x + 1\nf(5)"
  , testCase "Compile higher order functions" $ compiles "fn apply(f, x) = f(x)\napply((n) => n + 1, 5)"
  , testCase "Compile list operations" $ compiles "head([1, 2, 3])"
  , testCase "Compile string operations" $ compiles "concat(\"hello\", \" world\")"
  , testCase "Compile type comparisons" $ compiles "5 == 5 && true == true"
  , testCase "Compile nested if-else" $ compiles "if true { if false { 1 } else { 2 } } else { 3 }"
  , testCase "Compile sequence of statements" $ compiles "let x = 1\nlet y = 2\nx + y"
  , testCase "Compile procedure calls" $ compiles "fn proc(x) { let y = x }\nproc(42)"
  , testCase "Compile unary operations" $ compiles "-5 + !true"
  , testCase "Compile all arithmetic ops" $ compiles "1 + 2 - 3 * 4 / 5 % 2"
  , testCase "Compile all comparison ops" $ compiles "1 < 2 && 2 <= 2 && 3 > 2 && 3 >= 3"
  , testCase "Compile all boolean ops" $ compiles "true && false || !true"
  , testCase "Compile tuple creation" $ compiles "(1, 2, 3)"
  , testCase "Compile nested lists" $ compiles "[[1, 2], [3, 4]]"
  , testCase "Compile mixed expressions" $ compiles "let x = if true { [1, 2] } else { [3, 4] }\nhead(x)"
  , testCase "Compile return statement" $ compiles "fn f() = { RET 42 }\nf()"
  , testCase "Compile expression blocks" $ compiles "fn f() = { let x = 5 x + 10 }\nf()"
  , testCase "Compile multiple parameters" $ compiles "fn add3(a, b, c) = a + b + c\nadd3(1, 2, 3)"
  , testCase "Compile zero parameter function" $ compiles "fn const() = 42\nconst()"
  , testCase "Compile variable references" $ compiles "let x = 5\nlet y = x\ny"
  , testCase "Compile nested blocks" $ compiles "fn test() = { let x = 1 x + 2 }\ntest()"
  , testCase "Compile desugared pipes" $ compiles "5 |> (x) => x + 1"
  , testCase "Compile boolean literals" $ compiles "true && false"
  , testCase "Compile string literals" $ compiles "\"hello world\""
  , testCase "Compile empty strings" $ compiles "\"\""
  , testCase "Compile large integers" $ compiles "9223372036854775807"
  , testCase "Compile negative numbers" $ compiles "-42"
  , testCase "Compile division" $ compiles "10 / 2"
  , testCase "Compile modulo" $ compiles "10 % 3"
  ]

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling in Compilation"
  [ testCase "Compile with parse error doesn't crash" $ do
      case Parser.parseProgram "let = 5" of
        Left _ -> return ()  -- Expected
        Right _ -> assertFailure "Should have parse error"
  , testCase "Compile empty program" $ compiles ""
  , testCase "Compile only comments" $ compiles "// just a comment"
  , testCase "Compile with trailing newlines" $ compiles "42\n\n\n"
  , testCase "Compile with leading newlines" $ compiles "\n\n\n42"
  ]

additionalExpressionTests :: TestTree
additionalExpressionTests = testGroup "Additional Expression Compilation Tests"
  [ testCase "Compile unary minus" $ compiles "-42"
  , testCase "Compile unary NOT" $ compiles "!true"
  , testCase "Compile double NOT" $ compiles "!(!false)"
  , testCase "Compile unary on expression" $ compiles "-(5 + 3)"
  , testCase "Compile mixed unary" $ compiles "-5 + !true"
  , testCase "Compile parenthesized expression" $ compiles "(1 + 2) * 3"
  , testCase "Compile deeply nested parens" $ compiles "(((1 + 2)))"
  , testCase "Compile all comparison ops" $ compiles "1 < 2 && 2 <= 3 && 3 > 2 && 4 >= 4"
  , testCase "Compile equality chain" $ compiles "1 == 1 && 2 != 3"
  , testCase "Compile boolean arithmetic mix" $ compiles "(5 > 3) && (2 + 2 == 4)"
  , testCase "Compile string in expression" $ compiles "\"x\" + \"y\""
  , testCase "Compile list in expression" $ compiles "[1, 2] + [3, 4]"
  , testCase "Compile tuple access" $ compiles "(1, 2, 3)"
  , testCase "Compile variable in expression" $ compiles "let x = 5\nx + 10"
  , testCase "Compile multiple variables" $ compiles "let x = 5\nlet y = 10\nx + y"
  ]

additionalFunctionTests :: TestTree
additionalFunctionTests = testGroup "Additional Function Compilation Tests"
  [ testCase "Compile function with block" $ compiles "fn f(x) { let y = x y + 1 }"
  , testCase "Compile function returning function" $ compiles "fn makeAdder(x) = (y) => x + y"
  , testCase "Compile nested lambda" $ compiles "let f = (x) => (y) => x + y"
  , testCase "Compile function call in parameter" $ compiles "fn f(x) = x\nfn g(y) = f(y)\ng(5)"
  , testCase "Compile multiple function definitions" $ compiles "fn f(x) = x\nfn g(y) = y\nf(g(5))"
  , testCase "Compile procedure with side effects" $ compiles "fn proc(x) { let y = x }\nproc(5)"
  , testCase "Compile recursive function" $ compiles "fn fib(n) = if n <= 1 { 1 } else { fib(n-1) + fib(n-2) }\nfib(10)"
  , testCase "Compile tail recursive" $ compiles "fn sum(n, acc) = if n == 0 { acc } else { sum(n-1, acc+n) }\nsum(10, 0)"
  , testCase "Compile lambda with no params" $ compiles "let f = () => 42\nf()"
  , testCase "Compile function with many params" $ compiles "fn add5(a,b,c,d,e) = a+b+c+d+e\nadd5(1,2,3,4,5)"
  ]

additionalControlFlowTests :: TestTree
additionalControlFlowTests = testGroup "Additional Control Flow Compilation Tests"
  [ testCase "Compile if with boolean literal" $ compiles "if true { 1 } else { 0 }"
  , testCase "Compile if with false" $ compiles "if false { 1 } else { 0 }"
  , testCase "Compile if without else (implicit)" $ compiles "if true { 1 } else { 0 }"
  , testCase "Compile if with complex condition" $ compiles "let x = 5 > 3\nlet y = 2 < 4\nif x && y { 1 } else { 0 }"
  , testCase "Compile if with function call condition" $ compiles "fn isTrue() = true\nif isTrue() { 1 } else { 0 }"
  , testCase "Compile nested if in then" $ compiles "if true { if true { 1 } else { 2 } } else { 3 }"
  , testCase "Compile nested if in else" $ compiles "if false { 1 } else { if true { 2 } else { 3 } }"
  , testCase "Compile if-else chain" $ compiles "if false { 1 } else { if false { 2 } else { 3 } }"
  , testCase "Compile if with return" $ compiles "fn f(x) = if x > 0 { RET 1 } else { RET 0 }\nf(5)"
  , testCase "Compile if with multiple returns" $ compiles "fn f(x) { if x > 0 { RET 1 } else { RET 0 } }\nf(5)"
  ]

boxingTests :: TestTree
boxingTests = testGroup "Boxing Tests"
  [ testCase "Box integer" $ compiles "42"
  , testCase "Box boolean" $ compiles "true"
  , testCase "Box string" $ compiles "\"test\""
  , testCase "Box list" $ compiles "[1, 2, 3]"
  , testCase "Box tuple" $ compiles "(1, 2)"
  , testCase "Box function" $ compiles "(x) => x + 1"
  , testCase "Box nested structure" $ compiles "[(1, 2), (3, 4)]"
  , testCase "Unbox in arithmetic" $ compiles "let x = 5\nx + 10"
  , testCase "Unbox in comparison" $ compiles "let x = 5\nx > 3"
  , testCase "Unbox in function call" $ compiles "fn f(x) = x\nlet y = 5\nf(y)"
  ]

typeTests :: TestTree
typeTests = testGroup "Type Tests"
  [ testCase "Integer type" $ compiles "42"
  , testCase "Boolean type" $ compiles "true"
  , testCase "String type" $ compiles "\"hello\""
  , testCase "List type" $ compiles "[1, 2, 3]"
  , testCase "Tuple type" $ compiles "(1, 2, 3)"
  , testCase "Function type" $ compiles "fn f(x) = x"
  , testCase "Lambda type" $ compiles "(x) => x + 1"
  , testCase "Mixed types in list" $ compiles "[1, \"two\", true]"
  , testCase "Type in conditional" $ compiles "if isInt(5) { 1 } else { 0 }"
  , testCase "Type checking builtin" $ compiles "isBool(true)"
  ]

-- Helper functions
compiles :: String -> Assertion
compiles input = do
  case Parser.parseProgram input of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right ast -> do
      let result = Compiler.compileProgram ast
      length result `seq` return ()  -- Force evaluation to ensure it doesn't crash

compileToLLVM :: String -> String
compileToLLVM input = 
  case Parser.parseProgram input of
    Left _ -> ""
    Right ast -> Compiler.compileProgram ast
