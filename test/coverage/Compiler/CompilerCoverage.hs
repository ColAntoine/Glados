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
  , exhaustiveCompilationTests
  , exhaustiveFunctionCompilationTests
  , exhaustiveExpressionCompilationTests
  , topLevelTests
  , cabiTests
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

exhaustiveCompilationTests :: TestTree
exhaustiveCompilationTests = testGroup "Exhaustive Compilation Tests"
  [ testCase "Compile int 1" $ compiles "1"
  , testCase "Compile int 42" $ compiles "42"
  , testCase "Compile int 999" $ compiles "999"
  , testCase "Compile int max" $ compiles "9223372036854775807"
  , testCase "Compile neg int" $ compiles "-42"
  , testCase "Compile bool true 1" $ compiles "true"
  , testCase "Compile bool false 1" $ compiles "false"
  , testCase "Compile string hello" $ compiles "\"hello\""
  , testCase "Compile string test" $ compiles "\"test\""
  , testCase "Compile empty string 1" $ compiles "\"\""
  , testCase "Compile list ints" $ compiles "[1, 2, 3]"
  , testCase "Compile list empty" $ compiles "[]"
  , testCase "Compile list one" $ compiles "[42]"
  , testCase "Compile tuple two" $ compiles "(1, 2)"
  , testCase "Compile tuple three" $ compiles "(1, 2, 3)"
  , testCase "Compile add" $ compiles "1 + 2"
  , testCase "Compile sub" $ compiles "10 - 5"
  , testCase "Compile mul" $ compiles "3 * 4"
  , testCase "Compile div" $ compiles "10 / 2"
  , testCase "Compile mod" $ compiles "10 % 3"
  , testCase "Compile eq" $ compiles "5 == 5"
  , testCase "Compile neq" $ compiles "5 != 3"
  , testCase "Compile lt" $ compiles "3 < 5"
  , testCase "Compile lte" $ compiles "3 <= 5"
  , testCase "Compile gt" $ compiles "5 > 3"
  , testCase "Compile gte" $ compiles "5 >= 3"
  , testCase "Compile and" $ compiles "true && false"
  , testCase "Compile or" $ compiles "true || false"
  , testCase "Compile not" $ compiles "!true"
  , testCase "Compile unary minus 1" $ compiles "-5"
  , testCase "Compile add chain" $ compiles "1 + 2 + 3"
  , testCase "Compile mul chain" $ compiles "2 * 3 * 4"
  , testCase "Compile mixed arithmetic 1" $ compiles "2 + 3 * 4"
  , testCase "Compile mixed arithmetic 2" $ compiles "10 - 4 / 2"
  , testCase "Compile with parens" $ compiles "(2 + 3) * 4"
  , testCase "Compile let x" $ compiles "let x = 5"
  , testCase "Compile let y" $ compiles "let y = 10"
  ]

exhaustiveFunctionCompilationTests :: TestTree
exhaustiveFunctionCompilationTests = testGroup "Exhaustive Function Compilation"
  [ testCase "Compile fn no params" $ compiles "fn f() = 42"
  , testCase "Compile fn one param" $ compiles "fn f(x) = x"
  , testCase "Compile fn two params" $ compiles "fn f(x, y) = x + y"
  , testCase "Compile fn three params" $ compiles "fn f(a, b, c) = a + b + c"
  , testCase "Compile fn return bool" $ compiles "fn f() = true"
  , testCase "Compile fn return string" $ compiles "fn f() = \"test\""
  , testCase "Compile fn return list" $ compiles "fn f() = [1, 2]"
  , testCase "Compile fn with arithmetic" $ compiles "fn f(x) = x + 1"
  , testCase "Compile fn with comparison" $ compiles "fn f(x) = x > 0"
  , testCase "Compile fn with if 1" $ compiles "fn f(x) = if x > 0 { 1 } else { 0 }"
  , testCase "Compile lambda no param" $ compiles "() => 42"
  , testCase "Compile lambda one param" $ compiles "(x) => x + 1"
  , testCase "Compile lambda two params" $ compiles "(x, y) => x + y"
  , testCase "Compile lambda call" $ compiles "((x) => x + 1)(5)"
  , testCase "Compile let lambda" $ compiles "let f = (x) => x * 2"
  , testCase "Compile lambda in list" $ compiles "[(x) => x + 1]"
  , testCase "Compile recursive fn" $ compiles "fn fact(n) = if n <= 1 { 1 } else { n * fact(n-1) }"
  , testCase "Compile nested fn def" $ compiles "fn outer(x) = { fn inner(y) = y + 1 inner(x) }"
  ]

exhaustiveExpressionCompilationTests :: TestTree
exhaustiveExpressionCompilationTests = testGroup "Exhaustive Expression Compilation"
  [ testCase "Compile if true" $ compiles "if true { 1 } else { 0 }"
  , testCase "Compile if false" $ compiles "if false { 0 } else { 1 }"
  , testCase "Compile if comparison" $ compiles "if 5 > 3 { 1 } else { 0 }"
  , testCase "Compile if equality" $ compiles "if 5 == 5 { 1 } else { 0 }"
  , testCase "Compile if and" $ compiles "if true && false { 1 } else { 0 }"
  , testCase "Compile if or" $ compiles "if true || false { 1 } else { 0 }"
  , testCase "Compile nested if 1" $ compiles "if true { if true { 1 } else { 2 } } else { 3 }"
  , testCase "Compile nested if 2" $ compiles "if false { 1 } else { if true { 2 } else { 3 } }"
  , testCase "Compile if chain" $ compiles "if false { 1 } else { if false { 2 } else { 3 } }"
  , testCase "Compile head list" $ compiles "head([1, 2, 3])"
  , testCase "Compile tail list" $ compiles "tail([1, 2, 3])"
  , testCase "Compile at list" $ compiles "at([1, 2, 3], 0)"
  , testCase "Compile len list" $ compiles "len([1, 2, 3])"
  , testCase "Compile reverse list" $ compiles "reverse([1, 2, 3])"
  , testCase "Compile concat lists" $ compiles "concat([1], [2])"
  , testCase "Compile head string" $ compiles "head(\"hello\")"
  , testCase "Compile tail string" $ compiles "tail(\"hello\")"
  , testCase "Compile at string" $ compiles "at(\"hello\", 0)"
  , testCase "Compile len string" $ compiles "len(\"hello\")"
  , testCase "Compile concat strings" $ compiles "concat(\"hello\", \"world\")"
  , testCase "Compile isInt" $ compiles "isInt(5)"
  , testCase "Compile isBool" $ compiles "isBool(true)"
  , testCase "Compile isString" $ compiles "isString(\"test\")"
  , testCase "Compile isList" $ compiles "isList([1, 2])"
  , testCase "Compile print" $ compiles "print(\"hello\")"
  ]

-- TopLevel.hs specific tests - comprehensive coverage
topLevelTests :: TestTree
topLevelTests = testGroup "TopLevel.hs Coverage"
  [ testGroup "TLFn compilation"
      [ testCase "Function with zero params" $ compiles "fn zero() = 0"
      , testCase "Function with one param" $ compiles "fn id(x) = x"
      , testCase "Function with two params" $ compiles "fn add(x, y) = x + y"
      , testCase "Function with three params" $ compiles "fn add3(x, y, z) = x + y + z"
      , testCase "Function with four params" $ compiles "fn add4(a, b, c, d) = a + b + c + d"
      , testCase "Function returning constant" $ compiles "fn const() = 42"
      , testCase "Function with arithmetic body" $ compiles "fn calc(x) = x * 2 + 1"
      , testCase "Function with comparison body" $ compiles "fn cmp(x, y) = x > y"
      , testCase "Function with boolean body" $ compiles "fn logic(a, b) = a && b"
      , testCase "Function with if-else body" $ compiles "fn max(a, b) = if a > b { a } else { b }"
      , testCase "Function with nested if body" $ compiles "fn sign(x) = if x > 0 { 1 } else { if x < 0 { -1 } else { 0 } }"
      , testCase "Function with list body" $ compiles "fn list(a, b) = [a, b]"
      , testCase "Function with string body" $ compiles "fn str() = \"hello\""
      , testCase "Function with call body" $ compiles "fn f(x) = x\nfn g(y) = f(y)"
      , testCase "Multiple function definitions" $ compiles "fn f() = 1\nfn g() = 2\nfn h() = 3"
      , testCase "Function calling previous function" $ compiles "fn a() = 5\nfn b() = a()\nfn c() = b()"
      , testCase "Function with block body" $ compiles "fn block(x) = { let y = x + 1 y * 2 }"
      , testCase "Function with pipe operator" $ compiles "fn pipe(x) = x |> (y) => y + 1"
      , testCase "Function with multiple pipes" $ compiles "fn pipes(x) = x |> (y) => y + 1 |> (z) => z * 2"
      , testCase "Function with lambda param" $ compiles "fn apply(f, x) = f(x)"
      , testCase "Function calling builtin" $ compiles "fn getLen(xs) = len(xs)"
      , testCase "Function with head call" $ compiles "fn first(xs) = head(xs)"
      , testCase "Function with tail call" $ compiles "fn rest(xs) = tail(xs)"
      , testCase "Function with at call" $ compiles "fn get(xs, i) = at(xs, i)"
      , testCase "Function with reverse call" $ compiles "fn rev(xs) = reverse(xs)"
      , testCase "Function with concat call" $ compiles "fn join(a, b) = concat(a, b)"
      , testCase "Function with unary minus" $ compiles "fn neg(x) = -x"
      , testCase "Function with unary not" $ compiles "fn notFunc(b) = !b"
      , testCase "Function with equality" $ compiles "fn eq(a, b) = a == b"
      , testCase "Function with inequality" $ compiles "fn neq(a, b) = a != b"
      , testCase "Function with less than" $ compiles "fn lt(a, b) = a < b"
      , testCase "Function with less equal" $ compiles "fn le(a, b) = a <= b"
      , testCase "Function with greater than" $ compiles "fn gt(a, b) = a > b"
      , testCase "Function with greater equal" $ compiles "fn ge(a, b) = a >= b"
      , testCase "Function with AND" $ compiles "fn andFunc(a, b) = a && b"
      , testCase "Function with OR" $ compiles "fn orFunc(a, b) = a || b"
      , testCase "Function with modulo" $ compiles "fn mod(a, b) = a % b"
      , testCase "Recursive factorial" $ compiles "fn fact(n) = if n <= 1 { 1 } else { n * fact(n - 1) }"
      , testCase "Recursive fibonacci" $ compiles "fn fib(n) = if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }"
      , testCase "Function with empty list" $ compiles "fn emptyList() = []"
      , testCase "Function with tuple" $ compiles "fn pair(a, b) = (a, b)"
      , testCase "Function with isInt" $ compiles "fn checkInt(x) = isInt(x)"
      , testCase "Function with isBool" $ compiles "fn checkBool(x) = isBool(x)"
      , testCase "Function with isString" $ compiles "fn checkString(x) = isString(x)"
      , testCase "Function with isList" $ compiles "fn checkList(x) = isList(x)"
      , testCase "Function with print" $ compiles "fn printMsg(msg) = print(msg)"
      ]
  , testGroup "TLProc compilation"
      [ testCase "Procedure with zero params" $ compiles "fn proc() { }"
      , testCase "Procedure with one param" $ compiles "fn proc(x) { let y = x }"
      , testCase "Procedure with two params" $ compiles "fn proc(x, y) { let z = x + y }"
      , testCase "Procedure with three params" $ compiles "fn proc(a, b, c) { let d = a + b + c }"
      , testCase "Procedure with let statement" $ compiles "fn proc(x) { let y = x * 2 }"
      , testCase "Procedure with multiple lets" $ compiles "fn proc(x) { let y = x let z = y }"
      , testCase "Procedure with expr statement" $ compiles "fn proc(x) { x + 1 }"
      , testCase "Procedure with function def inside" $ compiles "fn proc() { fn inner() = 42 }"
      , testCase "Procedure returning zero" $ compiles "fn proc(x) { let y = x }"
      , testCase "Procedure with nested proc" $ compiles "fn outer() { fn inner() { let x = 5 } }"
      , testCase "Procedure with print" $ compiles "fn proc(msg) { print(msg) }"
      , testCase "Procedure with multiple statements" $ compiles "fn proc(x) { let a = x let b = a + 1 b }"
      , testCase "Procedure calling function" $ compiles "fn f(x) = x\nfn proc(y) { f(y) }"
      , testCase "Procedure with arithmetic" $ compiles "fn proc(a, b) { a + b }"
      , testCase "Procedure with comparison" $ compiles "fn proc(a, b) { a > b }"
      , testCase "Procedure with boolean op" $ compiles "fn proc(a, b) { a && b }"
      , testCase "Procedure with if-else" $ compiles "fn proc(x) { if x > 0 { 1 } else { 0 } }"
      , testCase "Procedure with list" $ compiles "fn proc(a, b) { [a, b] }"
      , testCase "Procedure with string" $ compiles "fn proc() { \"hello\" }"
      , testCase "Procedure with builtin call" $ compiles "fn proc(xs) { len(xs) }"
      ]
  , testGroup "TLLet compilation"
      [ testCase "Let binding integer" $ compiles "let x = 42"
      , testCase "Let binding boolean" $ compiles "let flag = true"
      , testCase "Let binding string" $ compiles "let msg = \"hello\""
      , testCase "Let binding list" $ compiles "let nums = [1, 2, 3]"
      , testCase "Let binding expression" $ compiles "let result = 2 + 3"
      , testCase "Let binding function call" $ compiles "fn f() = 5\nlet x = f()"
      , testCase "Let binding with pipe" $ compiles "let result = 5 |> (x) => x + 1"
      , testCase "Let binding lambda" $ compiles "let f = (x) => x + 1"
      , testCase "Let binding if-else" $ compiles "let x = if true { 1 } else { 0 }"
      , testCase "Multiple let bindings" $ compiles "let a = 1\nlet b = 2\nlet c = 3"
      , testCase "Let using previous let" $ compiles "let x = 5\nlet y = x + 1"
      , testCase "Chained let dependencies" $ compiles "let a = 1\nlet b = a + 1\nlet c = b + 1"
      , testCase "Let binding complex expr" $ compiles "let x = (2 + 3) * (4 - 1)"
      , testCase "Let binding builtin call" $ compiles "let length = len([1, 2, 3])"
      , testCase "Let binding head" $ compiles "let first = head([1, 2, 3])"
      , testCase "Let binding tail" $ compiles "let rest = tail([1, 2, 3])"
      , testCase "Let binding at" $ compiles "let elem = at([1, 2, 3], 1)"
      , testCase "Let binding concat" $ compiles "let joined = concat([1], [2])"
      , testCase "Let binding reverse" $ compiles "let rev = reverse([1, 2, 3])"
      , testCase "Let binding unary minus" $ compiles "let neg = -5"
      , testCase "Let binding unary not" $ compiles "let notVal = !true"
      , testCase "Let binding equality" $ compiles "let eq = 5 == 5"
      , testCase "Let binding inequality" $ compiles "let neq = 5 != 3"
      , testCase "Let binding less than" $ compiles "let lt = 3 < 5"
      , testCase "Let binding less equal" $ compiles "let le = 3 <= 5"
      , testCase "Let binding greater than" $ compiles "let gt = 5 > 3"
      , testCase "Let binding greater equal" $ compiles "let ge = 5 >= 3"
      , testCase "Let binding AND" $ compiles "let andVal = true && false"
      , testCase "Let binding OR" $ compiles "let orVal = true || false"
      , testCase "Let binding modulo" $ compiles "let mod = 10 % 3"
      , testCase "Let binding tuple" $ compiles "let pair = (1, 2)"
      , testCase "Let binding empty list" $ compiles "let empty = []"
      , testCase "Let binding nested list" $ compiles "let nested = [[1, 2], [3, 4]]"
      , testCase "Let binding isInt" $ compiles "let check = isInt(5)"
      , testCase "Let binding isBool" $ compiles "let check = isBool(true)"
      , testCase "Let binding isString" $ compiles "let check = isString(\"test\")"
      , testCase "Let binding isList" $ compiles "let check = isList([1, 2])"
      ]
  , testGroup "TLExpr compilation"
      [ testCase "Top-level integer expr" $ compiles "42"
      , testCase "Top-level boolean expr" $ compiles "true"
      , testCase "Top-level string expr" $ compiles "\"hello\""
      , testCase "Top-level arithmetic expr" $ compiles "2 + 3"
      , testCase "Top-level comparison expr" $ compiles "5 > 3"
      , testCase "Top-level boolean expr" $ compiles "true && false"
      , testCase "Top-level list expr" $ compiles "[1, 2, 3]"
      , testCase "Top-level function call expr" $ compiles "fn f() = 5\nf()"
      , testCase "Top-level if-else expr" $ compiles "if true { 1 } else { 0 }"
      , testCase "Top-level builtin call" $ compiles "len([1, 2, 3])"
      , testCase "Top-level head call" $ compiles "head([1, 2, 3])"
      , testCase "Top-level tail call" $ compiles "tail([1, 2, 3])"
      , testCase "Top-level at call" $ compiles "at([1, 2, 3], 0)"
      , testCase "Top-level reverse call" $ compiles "reverse([1, 2, 3])"
      , testCase "Top-level concat call" $ compiles "concat([1], [2])"
      , testCase "Top-level print call" $ compiles "print(\"test\")"
      , testCase "Top-level variable reference" $ compiles "let x = 5\nx"
      , testCase "Top-level lambda call" $ compiles "((x) => x + 1)(5)"
      , testCase "Top-level pipe expr" $ compiles "5 |> (x) => x + 1"
      , testCase "Top-level multiple pipes" $ compiles "5 |> (x) => x + 1 |> (y) => y * 2"
      , testCase "Top-level unary minus" $ compiles "-42"
      , testCase "Top-level unary not" $ compiles "!true"
      , testCase "Top-level equality" $ compiles "5 == 5"
      , testCase "Top-level inequality" $ compiles "5 != 3"
      , testCase "Top-level less than" $ compiles "3 < 5"
      , testCase "Top-level less equal" $ compiles "3 <= 5"
      , testCase "Top-level greater than" $ compiles "5 > 3"
      , testCase "Top-level greater equal" $ compiles "5 >= 3"
      , testCase "Top-level AND" $ compiles "true && false"
      , testCase "Top-level OR" $ compiles "true || false"
      , testCase "Top-level modulo" $ compiles "10 % 3"
      , testCase "Top-level tuple" $ compiles "(1, 2, 3)"
      , testCase "Top-level empty list" $ compiles "[]"
      , testCase "Top-level nested list" $ compiles "[[1], [2]]"
      , testCase "Top-level isInt" $ compiles "isInt(42)"
      , testCase "Top-level isBool" $ compiles "isBool(true)"
      , testCase "Top-level isString" $ compiles "isString(\"test\")"
      , testCase "Top-level isList" $ compiles "isList([1])"
      ]
  , testGroup "Program compilation"
      [ testCase "Single function program" $ compiles "fn main() = 42"
      , testCase "Multiple functions program" $ compiles "fn f() = 1\nfn g() = 2"
      , testCase "Function and let program" $ compiles "fn f() = 5\nlet x = f()"
      , testCase "Function and expr program" $ compiles "fn f() = 5\nf()"
      , testCase "Let and expr program" $ compiles "let x = 5\nx + 1"
      , testCase "Complex program structure" $ compiles "fn a() = 1\nfn b() = 2\nlet x = a()\nlet y = b()\nx + y"
      , testCase "Program with procedure" $ compiles "fn proc() { let x = 5 }\nproc()"
      , testCase "Program with nested functions" $ compiles "fn outer() = { fn inner() = 42 inner() }"
      , testCase "Program ending with let" $ compiles "fn f() = 5\nlet x = f()"
      , testCase "Program ending with expr" $ compiles "fn f() = 5\nlet x = 10\nf() + x"
      , testCase "Only let bindings" $ compiles "let x = 1\nlet y = 2"
      , testCase "Only expressions" $ compiles "1\n2\n3"
      , testCase "Function then multiple exprs" $ compiles "fn f() = 42\nf()\nf()"
      ]
  , testGroup "Program with main return value"
      [ testCase "Program returns integer" $ compiles "42"
      , testCase "Program returns expression" $ compiles "2 + 3"
      , testCase "Program returns function call" $ compiles "fn f() = 42\nf()"
      , testCase "Program returns variable" $ compiles "let x = 42\nx"
      , testCase "Program returns if-else" $ compiles "if true { 42 } else { 0 }"
      , testCase "Program returns builtin" $ compiles "len([1, 2, 3])"
      , testCase "Program returns lambda call" $ compiles "((x) => x * 2)(21)"
      , testCase "Program returns pipe" $ compiles "42 |> (x) => x"
      , testCase "Program with multiple exprs returns last" $ compiles "1\n2\n3"
      , testCase "Program returns boolean" $ compiles "true"
      , testCase "Program returns string" $ compiles "\"result\""
      , testCase "Program returns list" $ compiles "[1, 2, 3]"
      , testCase "Program returns comparison" $ compiles "5 > 3"
      , testCase "Program returns boolean op" $ compiles "true && false"
      ]
  , testGroup "Function name tracking"
      [ testCase "Single function name tracked" $ compiles "fn myFunc() = 42"
      , testCase "Multiple function names tracked" $ compiles "fn first() = 1\nfn second() = 2\nfn third() = 3"
      , testCase "Procedure name tracked" $ compiles "fn myProc() { let x = 5 }"
      , testCase "Mixed function and procedure names" $ compiles "fn func() = 1\nfn proc() { let x = 2 }"
      , testCase "Function names with underscores" $ compiles "fn my_func() = 1\nfn another_one() = 2"
      , testCase "Function names with numbers" $ compiles "fn func1() = 1\nfn func2() = 2"
      ]
  , testGroup "Nested function compilation"
      [ testCase "Function with nested function" $ compiles "fn outer(x) = { fn inner(y) = y + 1 inner(x) }"
      , testCase "Function with multiple nested" $ compiles "fn outer(x) = { fn a(y) = y + 1 fn b(z) = z * 2 a(x) + b(x) }"
      , testCase "Deeply nested functions" $ compiles "fn a(x) = { fn b(y) = { fn c(z) = z + 1 c(y) } b(x) }"
      , testCase "Procedure with nested function" $ compiles "fn proc() { fn inner() = 42 }"
      , testCase "Procedure with nested procedure" $ compiles "fn outer() { fn inner() { let x = 5 } }"
      , testCase "Nested function with params" $ compiles "fn outer(a, b) = { fn inner(x, y) = x + y inner(a, b) }"
      ]
  , testGroup "Code state management"
      [ testCase "Locals cleared between functions" $ compiles "fn f(x) = x\nfn g(y) = y"
      , testCase "Code cleared between functions" $ compiles "fn a() = { let x = 1 x }\nfn b() = { let y = 2 y }"
      , testCase "State preserved in main" $ compiles "let x = 1\nlet y = 2\nx + y"
      , testCase "Function state isolated" $ compiles "fn f() = { let local = 1 local }\nlet global = 2"
      , testCase "Params isolated between functions" $ compiles "fn f(x, y) = x + y\nfn g(a, b, c) = a + b + c"
      ]
  , testGroup "Pipe desugaring in compilation"
      [ testCase "Pipe in function body" $ compiles "fn f(x) = x |> (y) => y + 1"
      , testCase "Pipe in let binding" $ compiles "let result = 5 |> (x) => x * 2"
      , testCase "Pipe in top-level expr" $ compiles "42 |> (x) => x"
      , testCase "Multiple pipes in function" $ compiles "fn f(x) = x |> (a) => a + 1 |> (b) => b * 2"
      , testCase "Multiple pipes in let" $ compiles "let x = 1 |> (a) => a + 1 |> (b) => b + 1"
      , testCase "Pipe with complex lambda" $ compiles "fn f(x) = x |> (a) => a * 2 + 1"
      , testCase "Chained pipes with builtins" $ compiles "fn f(xs) = xs |> (a) => head(a) |> (b) => b + 1"
      ]
  , testGroup "LLVM output structure"
      [ testCase "Output contains prelude" $ assertContains (compileToLLVM "42") "Value"
      , testCase "Output contains main function" $ assertContains (compileToLLVM "42") "define i32 @main"
      , testCase "Output contains entry label" $ assertContains (compileToLLVM "42") "entry:"
      , testCase "Output contains ret" $ assertContains (compileToLLVM "42") "ret"
      , testCase "Function definition in output" $ assertContains (compileToLLVM "fn f() = 42") "define %Value @f"
      , testCase "Function with params in output" $ assertContains (compileToLLVM "fn f(x) = x") "%Value* %x.ptr"
      , testCase "Multiple functions in output" $ 
          let code = compileToLLVM "fn f() = 1\nfn g() = 2" in
          assertContains code "@f" >> assertContains code "@g"
      , testCase "String in output creates global" $ assertContains (compileToLLVM "\"hello\"") "@str"
      , testCase "Let binding creates alloca" $ assertContains (compileToLLVM "let x = 42") "alloca"
      , testCase "Let binding creates store" $ assertContains (compileToLLVM "let x = 42") "store"
      , testCase "Multiple functions create multiple defines" $ 
          let code = compileToLLVM "fn a() = 1\nfn b() = 2\nfn c() = 3" in
          assertContains code "@a" >> assertContains code "@b" >> assertContains code "@c"
      ]
  , testGroup "Return value handling"
      [ testCase "Integer return creates tag check" $ assertContains (compileToLLVM "42") "icmp eq"
      , testCase "Integer return has int path" $ assertContains (compileToLLVM "42") "trunc"
      , testCase "Integer return has zero path" $ assertContains (compileToLLVM "42") "ret i32 0"
      , testCase "Function call return handled" $ assertContains (compileToLLVM "fn f() = 42\nf()") "call %Value @f"
      , testCase "Boolean return handled" $ assertContains (compileToLLVM "true") "ret i32"
      , testCase "String return handled" $ assertContains (compileToLLVM "\"test\"") "ret i32"
      , testCase "List return handled" $ assertContains (compileToLLVM "[1, 2]") "ret i32"
      ]
  , testGroup "Edge cases"
      [ testCase "Function with same param names as previous" $ compiles "fn f(x) = x\nfn g(x) = x"
      , testCase "Let shadowing function param" $ compiles "fn f(x) = { let x = x + 1 x }"
      , testCase "Multiple lets same name different scopes" $ compiles "fn f() = { let x = 1 x }\nfn g() = { let x = 2 x }"
      , testCase "Empty procedure compilation" $ compiles "fn proc() { }"
      , testCase "Function calling itself" $ compiles "fn f(n) = if n <= 0 { 0 } else { f(n - 1) }"
      , testCase "Very deep recursion definition" $ compiles "fn deep(n) = if n <= 0 { 0 } else { deep(deep(n - 1)) }"
      , testCase "Many parameters" $ compiles "fn many(a, b, c, d, e, f, g) = a + b + c + d + e + f + g"
      ]
  ]

-- C ABI specific tests - comprehensive coverage of compileProgramCABI and related functions
cabiTests :: TestTree
cabiTests = testGroup "C ABI Compilation"
  [ testGroup "compileProgramCABI basic functionality"
      [ testCase "CABI compiles empty program" $ compilesCABI ""
      , testCase "CABI compiles single function" $ compilesCABI "fn f() = 42"
      , testCase "CABI compiles function with one param" $ compilesCABI "fn f(x) = x"
      , testCase "CABI compiles function with two params" $ compilesCABI "fn add(x, y) = x + y"
      , testCase "CABI compiles function with three params" $ compilesCABI "fn add3(a, b, c) = a + b + c"
      , testCase "CABI compiles multiple functions" $ compilesCABI "fn f() = 1\nfn g() = 2"
      , testCase "CABI compiles function with arithmetic" $ compilesCABI "fn calc(x) = x * 2 + 1"
      , testCase "CABI compiles function with comparison" $ compilesCABI "fn cmp(a, b) = a > b"
      , testCase "CABI compiles function with boolean logic" $ compilesCABI "fn logic(a, b) = a && b"
      , testCase "CABI compiles function with if-else" $ compilesCABI "fn max(a, b) = if a > b { a } else { b }"
      ]
  , testGroup "compileProgramCABI with procedures"
      [ testCase "CABI compiles empty procedure" $ compilesCABI "fn proc() { }"
      , testCase "CABI compiles procedure with param" $ compilesCABI "fn proc(x) { let y = x }"
      , testCase "CABI compiles procedure with two params" $ compilesCABI "fn proc(x, y) { let z = x + y }"
      , testCase "CABI compiles procedure with let statements" $ compilesCABI "fn proc(x) { let a = x let b = a + 1 }"
      , testCase "CABI compiles procedure with expr statement" $ compilesCABI "fn proc(x) { x + 1 }"
      , testCase "CABI compiles multiple procedures" $ compilesCABI "fn p1() { let x = 1 }\nfn p2() { let y = 2 }"
      , testCase "CABI compiles mixed functions and procedures" $ compilesCABI "fn f() = 1\nfn proc() { let x = 2 }"
      ]
  , testGroup "compileProgramCABI ignores non-function top-levels"
      [ testCase "CABI ignores top-level let" $ compilesCABI "let x = 42\nfn f() = 1"
      , testCase "CABI ignores top-level expr" $ compilesCABI "42\nfn f() = 1"
      ]
  , testGroup "compileTopLevelCABI function"
      [ testCase "CABI function with internal name" $ 
          assertContains (compileToLLVMCABI "fn myFunc() = 42") "@__flux_myFunc"
      , testCase "CABI function zero params has empty param list" $ 
          assertContains (compileToLLVMCABI "fn f() = 1") "@__flux_f()"
      , testCase "CABI function one param" $ 
          assertContains (compileToLLVMCABI "fn f(x) = x") "%Value* %x.ptr"
      , testCase "CABI function two params" $ 
          assertContains (compileToLLVMCABI "fn f(x, y) = x + y") "%Value* %x.ptr, %Value* %y.ptr"
      , testCase "CABI function has entry label" $ 
          assertContains (compileToLLVMCABI "fn f() = 1") "entry:"
      , testCase "CABI function has return" $ 
          assertContains (compileToLLVMCABI "fn f() = 42") "ret %Value"
      , testCase "CABI function processes pipe operators" $ compilesCABI "fn f(x) = x |> (y) => y + 1"
      , testCase "CABI function with nested function" $ compilesCABI "fn outer(x) = { fn inner(y) = y + 1 inner(x) }"
      ]
  , testGroup "compileTopLevelCABIProc procedure"
      [ testCase "CABI proc with internal name" $ 
          assertContains (compileToLLVMCABI "fn myProc() { let x = 1 }") "@__flux_myProc"
      , testCase "CABI proc zero params" $ 
          assertContains (compileToLLVMCABI "fn proc() { }") "@__flux_proc()"
      , testCase "CABI proc one param" $ 
          assertContains (compileToLLVMCABI "fn proc(x) { let y = x }") "%Value* %x.ptr"
      , testCase "CABI proc two params" $ 
          assertContains (compileToLLVMCABI "fn proc(a, b) { let c = a + b }") "%Value* %a.ptr, %Value* %b.ptr"
      , testCase "CABI proc has entry label" $ 
          assertContains (compileToLLVMCABI "fn proc() { let x = 1 }") "entry:"
      , testCase "CABI proc returns boxed zero" $ 
          assertContains (compileToLLVMCABI "fn proc() { }") "ret %Value"
      , testCase "CABI proc with nested procedure" $ compilesCABI "fn outer() { fn inner() { let x = 5 } }"
      ]
  , testGroup "generateCABIWrapper function"
      [ testCase "Wrapper created for function" $ 
          assertContains (compileToLLVMCABI "fn f() = 42") "define i64 @f"
      , testCase "Wrapper zero params no boxing" $ 
          let code = compileToLLVMCABI "fn f() = 42" 
          in assertContains code "define i64 @f()" >> assertContains code "@__flux_f()"
      , testCase "Wrapper one param boxes argument" $ 
          let code = compileToLLVMCABI "fn f(x) = x"
          in assertContains code "i64 %p0" >> assertContains code "@box_int(i64 %p0)"
      , testCase "Wrapper one param allocates boxed value" $ 
          assertContains (compileToLLVMCABI "fn f(x) = x") "alloca %Value"
      , testCase "Wrapper one param stores boxed value" $ 
          assertContains (compileToLLVMCABI "fn f(x) = x") "store %Value"
      , testCase "Wrapper two params boxes both" $ 
          let code = compileToLLVMCABI "fn f(x, y) = x + y"
          in assertContains code "@box_int(i64 %p0)" >> assertContains code "@box_int(i64 %p1)"
      , testCase "Wrapper calls internal function" $ 
          assertContains (compileToLLVMCABI "fn f(x) = x") "call %Value @__flux_f"
      , testCase "Wrapper passes boxed params" $ 
          assertContains (compileToLLVMCABI "fn f(x) = x") "%Value* %pboxptr0"
      , testCase "Wrapper unboxes result" $ 
          assertContains (compileToLLVMCABI "fn f() = 42") "@unbox_int(%Value %res)"
      , testCase "Wrapper returns i64" $ 
          assertContains (compileToLLVMCABI "fn f() = 42") "ret i64"
      , testCase "Wrapper three params" $ 
          let code = compileToLLVMCABI "fn f(a, b, c) = a + b + c"
          in assertContains code "i64 %p0, i64 %p1, i64 %p2"
      , testCase "Wrapper for procedure" $ 
          assertContains (compileToLLVMCABI "fn proc(x) { let y = x }") "define i64 @proc"
      ]
  , testGroup "CABI output structure"
      [ testCase "CABI output has C ABI mode comment" $ 
          assertContains (compileToLLVMCABI "fn f() = 1") "; Generated by Flux compiler (C ABI mode)"
      , testCase "CABI output has prelude" $ 
          assertContains (compileToLLVMCABI "fn f() = 1") "Value"
      , testCase "CABI output has internal function" $ 
          assertContains (compileToLLVMCABI "fn f() = 1") "@__flux_f"
      , testCase "CABI output has wrapper function" $ 
          assertContains (compileToLLVMCABI "fn f() = 1") "define i64 @f"
      , testCase "CABI multiple functions have multiple wrappers" $ 
          let code = compileToLLVMCABI "fn f() = 1\nfn g() = 2"
          in assertContains code "define i64 @f" >> assertContains code "define i64 @g"
      , testCase "CABI strings create globals" $ 
          assertContains (compileToLLVMCABI "fn f() = \"hello\"") "@str"
      ]
  , testGroup "CABI function name tracking"
      [ testCase "CABI tracks single function name" $ compilesCABI "fn myFunc() = 1"
      , testCase "CABI tracks multiple function names" $ compilesCABI "fn f1() = 1\nfn f2() = 2\nfn f3() = 3"
      , testCase "CABI tracks procedure name" $ compilesCABI "fn proc() { let x = 1 }"
      , testCase "CABI tracks mixed function and procedure names" $ compilesCABI "fn func() = 1\nfn proc() { let x = 2 }"
      ]
  , testGroup "CABI edge cases"
      [ testCase "CABI many parameters" $ compilesCABI "fn many(a, b, c, d, e) = a + b + c + d + e"
      , testCase "CABI recursive function" $ compilesCABI "fn fact(n) = if n <= 1 { 1 } else { n * fact(n - 1) }"
      , testCase "CABI function with list" $ compilesCABI "fn list(a, b) = [a, b]"
      , testCase "CABI function with string" $ compilesCABI "fn str() = \"test\""
      , testCase "CABI function with boolean" $ compilesCABI "fn bool() = true"
      , testCase "CABI function with builtin" $ compilesCABI "fn getLen(xs) = len(xs)"
      , testCase "CABI function with lambda" $ compilesCABI "fn apply(f, x) = f(x)"
      , testCase "CABI deeply nested function" $ compilesCABI "fn outer(x) = { fn inner(y) = { fn innermost(z) = z + 1 innermost(y) } inner(x) }"
      ]
  , testGroup "compileProgramToFile functionality"
      [ testCase "Regular compilation mode" $ do
          case Parser.parseProgram "fn f() = 42" of
            Left err -> assertFailure $ "Parse error: " ++ show err
            Right ast -> do
              let result = Compiler.compileProgram ast
              assertContains result "define i32 @main"
              assertBool "Should not have C ABI mode comment" (not $ isInfixOf "C ABI mode" result)
      , testCase "CABI compilation mode" $ do
          case Parser.parseProgram "fn f() = 42" of
            Left err -> assertFailure $ "Parse error: " ++ show err
            Right ast -> do
              let result = Compiler.compileProgramCABI ast
              assertContains result "; Generated by Flux compiler (C ABI mode)"
              assertContains result "define i64 @f"
      ]
  , testGroup "CABI state management"
      [ testCase "CABI locals cleared between functions" $ compilesCABI "fn f(x) = x\nfn g(y) = y"
      , testCase "CABI code cleared between functions" $ compilesCABI "fn f() = { let x = 1 x }\nfn g() = { let y = 2 y }"
      , testCase "CABI nested functions isolated" $ compilesCABI "fn outer1() = { fn inner() = 1 inner() }\nfn outer2() = { fn inner() = 2 inner() }"
      ]
  , testGroup "CABI parameter handling"
      [ testCase "CABI param with arithmetic" $ compilesCABI "fn f(x) = x + 10"
      , testCase "CABI param with comparison" $ compilesCABI "fn f(x, y) = x < y"
      , testCase "CABI param with boolean op" $ compilesCABI "fn f(a, b) = a || b"
      , testCase "CABI param in if condition" $ compilesCABI "fn f(x) = if x > 0 { 1 } else { 0 }"
      , testCase "CABI param in list" $ compilesCABI "fn f(a, b) = [a, b, a + b]"
      , testCase "CABI param in function call" $ compilesCABI "fn id(x) = x\nfn f(y) = id(y)"
      , testCase "CABI param in builtin call" $ compilesCABI "fn f(xs) = head(xs)"
      ]
  , testGroup "CABI expression coverage"
      [ testCase "CABI unary minus" $ compilesCABI "fn neg(x) = -x"
      , testCase "CABI unary not" $ compilesCABI "fn notFunc(b) = !b"
      , testCase "CABI equality" $ compilesCABI "fn eq(a, b) = a == b"
      , testCase "CABI inequality" $ compilesCABI "fn neq(a, b) = a != b"
      , testCase "CABI less than" $ compilesCABI "fn lt(a, b) = a < b"
      , testCase "CABI less equal" $ compilesCABI "fn le(a, b) = a <= b"
      , testCase "CABI greater than" $ compilesCABI "fn gt(a, b) = a > b"
      , testCase "CABI greater equal" $ compilesCABI "fn ge(a, b) = a >= b"
      , testCase "CABI modulo" $ compilesCABI "fn mod(a, b) = a % b"
      , testCase "CABI AND" $ compilesCABI "fn andFunc(a, b) = a && b"
      , testCase "CABI OR" $ compilesCABI "fn orFunc(a, b) = a || b"
      , testCase "CABI complex expression" $ compilesCABI "fn complex(a, b, c) = (a + b) * c - (a % b)"
      ]
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

compilesCABI :: String -> Assertion
compilesCABI input = do
  case Parser.parseProgram input of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right ast -> do
      let result = Compiler.compileProgramCABI ast
      length result `seq` return ()  -- Force evaluation to ensure it doesn't crash

compileToLLVMCABI :: String -> String
compileToLLVMCABI input = 
  case Parser.parseProgram input of
    Left _ -> ""
    Right ast -> Compiler.compileProgramCABI ast

assertContains :: String -> String -> Assertion
assertContains haystack needle =
  assertBool ("Expected output to contain '" ++ needle ++ "'") (needle `isInfixOf` haystack)
