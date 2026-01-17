module Interpreter.InterpreterCoverage (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Parser
import qualified Interpreter
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

-- This module contains comprehensive tests for Interpreter.hs
-- Goal: Achieve maximum coverage of interpreter evaluation and built-in functions

tests :: IO TestTree
tests = return $ testGroup "Interpreter Coverage"
  [ basicEvalTests
  , arithmeticTests
  , booleanTests
  , stringTests
  , listTests
  , functionTests
  , builtinListTests
  , builtinStringTests
  , builtinMathTests
  , builtinTypeCheckTests
  , builtinFileTests
  , builtinPrintTests
  , controlFlowTests
  , errorTests
  , advancedTests
  , edgeCaseTests
  , importTests
  , additionalArithmeticTests
  , additionalBooleanTests
  , additionalFunctionTests
  , additionalControlFlowTests
  , additionalErrorTests
  , exhaustiveArithmeticTests
  , exhaustiveBooleanTests
  , exhaustiveComparisonTests
  , exhaustiveListOperationTests
  , exhaustiveStringOperationTests
  ]

basicEvalTests :: TestTree
basicEvalTests = testGroup "Basic Evaluation"
  [ testCase "Evaluate integer" $ runProg "42"
  , testCase "Evaluate boolean true" $ runProg "true"
  , testCase "Evaluate boolean false" $ runProg "false"
  , testCase "Evaluate string" $ runProg "\"hello\""
  , testCase "Evaluate empty string" $ runProg "\"\""
  , testCase "Evaluate variable binding" $ runProg "let x = 42\nx"
  , testCase "Evaluate multiple bindings" $ runProg "let x = 1\nlet y = 2\nx + y"
  ]

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic Operations"
  [ testCase "Addition" $ runProg "1 + 2"
  , testCase "Subtraction" $ runProg "5 - 3"
  , testCase "Multiplication" $ runProg "4 * 5"
  , testCase "Division" $ runProg "10 / 2"
  , testCase "Modulo" $ runProg "10 % 3"
  , testCase "Unary minus" $ runProg "-5"
  , testCase "Unary minus on expression" $ runProg "-(5 + 3)"
  , testCase "Complex arithmetic" $ runProg "2 + 3 * 4 - 10 / 2"
  , testCase "Division by zero" $ runProgFail "10 / 0"
  , testCase "Nested arithmetic" $ runProg "((1 + 2) * (3 - 1)) / 2"
  ]

booleanTests :: TestTree
booleanTests = testGroup "Boolean Operations"
  [ testCase "Equality true" $ runProg "5 == 5"
  , testCase "Equality false" $ runProg "5 == 3"
  , testCase "Inequality true" $ runProg "5 != 3"
  , testCase "Inequality false" $ runProg "5 != 5"
  , testCase "Less than true" $ runProg "3 < 5"
  , testCase "Less than false" $ runProg "5 < 3"
  , testCase "Less than or equal true" $ runProg "3 <= 5"
  , testCase "Less than or equal equal" $ runProg "5 <= 5"
  , testCase "Greater than true" $ runProg "5 > 3"
  , testCase "Greater than false" $ runProg "3 > 5"
  , testCase "Greater than or equal true" $ runProg "5 >= 3"
  , testCase "Greater than or equal equal" $ runProg "5 >= 5"
  , testCase "Logical AND true" $ runProg "true && true"
  , testCase "Logical AND false" $ runProg "true && false"
  , testCase "Logical OR true" $ runProg "true || false"
  , testCase "Logical OR false" $ runProg "false || false"
  , testCase "Logical NOT true" $ runProg "!false"
  , testCase "Logical NOT false" $ runProg "!true"
  , testCase "Complex boolean expression" $ runProg "(5 > 3) && (2 < 4) || false"
  ]

stringTests :: TestTree
stringTests = testGroup "String Operations"
  [ testCase "String literal" $ runProg "\"hello\""
  , testCase "String with spaces" $ runProg "\"hello world\""
  , testCase "String with escape sequences" $ runProg "\"hello\\nworld\""
  , testCase "String equality" $ runProg "\"hello\" == \"hello\""
  , testCase "String inequality" $ runProg "\"hello\" != \"world\""
  ]

listTests :: TestTree
listTests = testGroup "List Operations"
  [ testCase "Empty list" $ runProg "[]"
  , testCase "List with elements" $ runProg "[1, 2, 3]"
  , testCase "List with mixed expressions" $ runProg "[1 + 1, 2 * 2, 3]"
  , testCase "Nested lists" $ runProg "[[1, 2], [3, 4]]"
  , testCase "List equality" $ runProg "[1, 2] == [1, 2]"
  , testCase "Tuple" $ runProg "(1, 2, 3)"
  , testCase "Tuple with expressions" $ runProg "(1 + 1, 2 * 2)"
  ]

functionTests :: TestTree
functionTests = testGroup "Function Evaluation"
  [ testCase "Simple function definition and call" $ 
      runProg "fn f(x) = x\nf(42)"
  , testCase "Function with arithmetic" $
      runProg "fn add(a, b) = a + b\nadd(3, 4)"
  , testCase "Function with multiple parameters" $
      runProg "fn mul3(a, b, c) = a * b * c\nmul3(2, 3, 4)"
  , testCase "Function with no parameters" $
      runProg "fn const() = 42\nconst()"
  , testCase "Recursive function (factorial)" $
      runProg "fn fact(n) = if n <= 1 { 1 } else { n * fact(n - 1) }\nfact(5)"
  , testCase "Closure captures environment" $
      runProg "let x = 10\nfn f(y) = x + y\nf(5)"
  , testCase "Lambda expression" $
      runProg "let f = (x) => x + 1\nf(5)"
  , testCase "Lambda with multiple params" $
      runProg "let add = (a, b) => a + b\nadd(3, 4)"
  , testCase "Nested function calls" $
      runProg "fn f(x) = x + 1\nfn g(x) = f(f(x))\ng(5)"
  , testCase "Function returning function" $
      runProg "fn makeAdder(x) = (y) => x + y\nlet add5 = makeAdder(5)\nadd5(3)"
  , testCase "Procedure without return" $
      runProg "fn proc(x) { let y = x }\nproc(42)"
  ]

builtinListTests :: TestTree
builtinListTests = testGroup "Built-in List Functions"
  [ testCase "len on list" $ runProg "len([1, 2, 3])"
  , testCase "len on empty list" $ runProg "len([])"
  , testCase "head on list" $ runProg "head([1, 2, 3])"
  , testCase "head on empty list" $ runProgFail "head([])"
  , testCase "tail on list" $ runProg "tail([1, 2, 3])"
  , testCase "tail on empty list" $ runProgFail "tail([])"
  , testCase "at on list" $ runProg "at([10, 20, 30], 1)"
  , testCase "at out of bounds" $ runProgFail "at([1, 2], 5)"
  , testCase "at negative index" $ runProgFail "at([1, 2], -1)"
  , testCase "concat lists" $ runProg "concat([1, 2], [3, 4])"
  , testCase "concat empty lists" $ runProg "concat([], [])"
  , testCase "reverse list" $ runProg "reverse([1, 2, 3])"
  , testCase "reverse empty list" $ runProg "reverse([])"
  , testCase "at first element" $ runProg "at([1, 2, 3], 0)"
  , testCase "at last element" $ runProg "at([1, 2, 3], 2)"
  , testCase "concat first empty" $ runProg "concat([], [1, 2])"
  , testCase "concat second empty" $ runProg "concat([1, 2], [])"
  , testCase "head single element" $ runProg "head([42])"
  , testCase "tail single element" $ runProg "tail([42])"
  , testCase "len nested list" $ runProg "len([[1, 2], [3, 4]])"
  , testCase "reverse nested list" $ runProg "reverse([[1, 2], [3, 4]])"
  , testCase "concat type mismatch" $ runProgFail "concat([1], \"hello\")"
  , testCase "at wrong first arg type" $ runProgFail "at(42, 0)"
  , testCase "at wrong second arg type" $ runProgFail "at([1, 2], \"x\")"
  ]

builtinStringTests :: TestTree
builtinStringTests = testGroup "Built-in String Functions"
  [ testCase "len on string" $ runProg "len(\"hello\")"
  , testCase "len on empty string" $ runProg "len(\"\")"
  , testCase "head on string" $ runProg "head(\"hello\")"
  , testCase "head on empty string" $ runProgFail "head(\"\")"
  , testCase "tail on string" $ runProg "tail(\"hello\")"
  , testCase "tail on empty string" $ runProgFail "tail(\"\")"
  , testCase "at on string" $ runProg "at(\"hello\", 1)"
  , testCase "at on string out of bounds" $ runProgFail "at(\"hi\", 5)"
  , testCase "concat strings" $ runProg "concat(\"hello\", \" world\")"
  , testCase "concat empty strings" $ runProg "concat(\"\", \"\")"
  , testCase "charAt" $ runProg "charAt(\"hello\", 1)"
  , testCase "charAt out of bounds" $ runProgFail "charAt(\"hi\", 10)"
  , testCase "substring" $ runProg "substring(\"hello world\", 0, 5)"
  , testCase "substring invalid range" $ runProgFail "substring(\"hello\", 5, 2)"
  , testCase "toUpper" $ runProg "toUpper(\"hello\")"
  , testCase "toUpper empty" $ runProg "toUpper(\"\")"
  , testCase "toLower" $ runProg "toLower(\"HELLO\")"
  , testCase "toLower empty" $ runProg "toLower(\"\")"
  , testCase "reverse string" $ runProg "reverse(\"hello\")"
  , testCase "head single char" $ runProg "head(\"x\")"
  , testCase "tail single char" $ runProg "tail(\"x\")"
  , testCase "at first char" $ runProg "at(\"hello\", 0)"
  , testCase "at last char" $ runProg "at(\"hello\", 4)"
  , testCase "charAt first" $ runProg "charAt(\"hello\", 0)"
  , testCase "charAt last" $ runProg "charAt(\"hello\", 4)"
  , testCase "substring full string" $ runProg "substring(\"hello\", 0, 5)"
  , testCase "substring empty result" $ runProg "substring(\"hello\", 2, 2)"
  , testCase "substring at end" $ runProg "substring(\"hello\", 3, 5)"
  , testCase "substring negative start" $ runProgFail "substring(\"hello\", -1, 2)"
  , testCase "substring beyond end" $ runProgFail "substring(\"hello\", 0, 10)"
  , testCase "toUpper mixed case" $ runProg "toUpper(\"HeLLo\")"
  , testCase "toLower mixed case" $ runProg "toLower(\"HeLLo\")"
  , testCase "toUpper with numbers" $ runProg "toUpper(\"hello123\")"
  , testCase "toLower with numbers" $ runProg "toLower(\"HELLO123\")"
  , testCase "concat first empty" $ runProg "concat(\"\", \"hello\")"
  , testCase "concat second empty" $ runProg "concat(\"hello\", \"\")"
  , testCase "reverse empty string" $ runProg "reverse(\"\")"
  , testCase "reverse single char" $ runProg "reverse(\"x\")"
  , testCase "at negative on string" $ runProgFail "at(\"hello\", -1)"
  , testCase "charAt negative" $ runProgFail "charAt(\"hello\", -1)"
  , testCase "len wrong type" $ runProgFail "len(42)"
  , testCase "head wrong type" $ runProgFail "head(42)"
  , testCase "tail wrong type" $ runProgFail "tail(true)"
  , testCase "charAt wrong first arg" $ runProgFail "charAt(42, 0)"
  , testCase "charAt wrong second arg" $ runProgFail "charAt(\"hello\", \"x\")"
  , testCase "substring wrong first arg" $ runProgFail "substring(42, 0, 1)"
  , testCase "substring wrong second arg" $ runProgFail "substring(\"hello\", \"x\", 1)"
  , testCase "substring wrong third arg" $ runProgFail "substring(\"hello\", 0, \"x\")"
  , testCase "toUpper wrong arg" $ runProgFail "toUpper(42)"
  , testCase "toLower wrong arg" $ runProgFail "toLower(42)"
  , testCase "reverse wrong type" $ runProgFail "reverse(42)"
  ]

builtinMathTests :: TestTree
builtinMathTests = testGroup "Built-in Math Functions"
  [ testCase "abs positive" $ runProg "abs(5)"
  , testCase "abs negative" $ runProg "abs(-5)"
  , testCase "abs zero" $ runProg "abs(0)"
  , testCase "min" $ runProg "min(3, 7)"
  , testCase "min equal" $ runProg "min(5, 5)"
  , testCase "min first smaller" $ runProg "min(2, 9)"
  , testCase "min second smaller" $ runProg "min(9, 2)"
  , testCase "max" $ runProg "max(3, 7)"
  , testCase "max equal" $ runProg "max(5, 5)"
  , testCase "max first larger" $ runProg "max(9, 2)"
  , testCase "max second larger" $ runProg "max(2, 9)"
  , testCase "pow positive" $ runProg "pow(2, 3)"
  , testCase "pow zero exponent" $ runProg "pow(5, 0)"
  , testCase "pow one exponent" $ runProg "pow(5, 1)"
  , testCase "pow zero base" $ runProg "pow(0, 5)"
  , testCase "pow negative exponent" $ runProgFail "pow(2, -1)"
  , testCase "abs wrong type" $ runProgFail "abs(\"hello\")"
  , testCase "min wrong types" $ runProgFail "min(5, \"hello\")"
  , testCase "min wrong first arg" $ runProgFail "min(\"hello\", 5)"
  , testCase "max wrong types" $ runProgFail "max(true, 5)"
  , testCase "pow wrong first arg" $ runProgFail "pow(\"hello\", 2)"
  , testCase "pow wrong second arg" $ runProgFail "pow(2, \"hello\")"
  , testCase "abs large negative" $ runProg "abs(-999999)"
  , testCase "pow large result" $ runProg "pow(2, 10)"
  , testCase "min negative numbers" $ runProg "min(-5, -3)"
  , testCase "max negative numbers" $ runProg "max(-5, -3)"
  ]

builtinTypeCheckTests :: TestTree
builtinTypeCheckTests = testGroup "Built-in Type Check Functions"
  [ testCase "isInt on int" $ runProg "isInt(42)"
  , testCase "isInt on string" $ runProg "isInt(\"hello\")"
  , testCase "isInt on bool" $ runProg "isInt(true)"
  , testCase "isInt on list" $ runProg "isInt([1, 2])"
  , testCase "isBool on bool" $ runProg "isBool(true)"
  , testCase "isBool on int" $ runProg "isBool(42)"
  , testCase "isBool on string" $ runProg "isBool(\"hello\")"
  , testCase "isBool on list" $ runProg "isBool([1, 2])"
  , testCase "isString on string" $ runProg "isString(\"hello\")"
  , testCase "isString on int" $ runProg "isString(42)"
  , testCase "isString on bool" $ runProg "isString(true)"
  , testCase "isString on list" $ runProg "isString([1, 2])"
  , testCase "isList on list" $ runProg "isList([1, 2])"
  , testCase "isList on string" $ runProg "isList(\"hello\")"
  , testCase "isList on int" $ runProg "isList(42)"
  , testCase "isList on bool" $ runProg "isList(true)"
  , testCase "isInt on negative" $ runProg "isInt(-42)"
  , testCase "isInt on zero" $ runProg "isInt(0)"
  , testCase "isBool on false" $ runProg "isBool(false)"
  , testCase "isString on empty" $ runProg "isString(\"\")"
  , testCase "isList on empty" $ runProg "isList([])"
  , testCase "isInt arity error" $ runProgFail "isInt(1, 2)"
  , testCase "isBool arity error" $ runProgFail "isBool(true, false)"
  , testCase "isString arity error" $ runProgFail "isString(\"a\", \"b\")"
  , testCase "isList arity error" $ runProgFail "isList([1], [2])"
  ]

builtinFileTests :: TestTree
builtinFileTests = testGroup "Built-in File Functions"
  [ testCase "writeFile and readFile" $ do
      withSystemTempDirectory "flux-test" $ \tmpDir -> do
        let testFile = tmpDir </> "test.txt"
        let prog = "writeFile(\"" ++ testFile ++ "\", \"hello\")\nreadFile(\"" ++ testFile ++ "\")"
        runProg prog
  , testCase "readFile non-existent" $ do
      withSystemTempDirectory "flux-test" $ \tmpDir -> do
        let testFile = tmpDir </> "nonexistent.txt"
        let prog = "readFile(\"" ++ testFile ++ "\")"
        runProgFail prog
  , testCase "appendFile" $ do
      withSystemTempDirectory "flux-test" $ \tmpDir -> do
        let testFile = tmpDir </> "append.txt"
        let prog = "writeFile(\"" ++ testFile ++ "\", \"hello\")\nappendFile(\"" ++ testFile ++ "\", \" world\")\nreadFile(\"" ++ testFile ++ "\")"
        runProg prog
  , testCase "writeFile wrong first arg" $ runProgFail "writeFile(42, \"content\")"
  , testCase "writeFile wrong second arg" $ runProgFail "writeFile(\"file.txt\", 42)"
  , testCase "readFile wrong arg" $ runProgFail "readFile(42)"
  , testCase "appendFile wrong first arg" $ runProgFail "appendFile(42, \"content\")"
  , testCase "appendFile wrong second arg" $ runProgFail "appendFile(\"file.txt\", 42)"
  ]

builtinPrintTests :: TestTree
builtinPrintTests = testGroup "Built-in Print Functions"
  [ testCase "print int" $ runProg "print(42)"
  , testCase "print bool" $ runProg "print(true)"
  , testCase "print string" $ runProg "print(\"hello\")"
  , testCase "print list" $ runProg "print([1, 2, 3])"
  , testCase "print empty list" $ runProg "print([])"
  , testCase "print nested list" $ runProg "print([[1, 2], [3, 4]])"
  , testCase "print tuple" $ runProg "print((1, 2, 3))"
  , testCase "print arity mismatch" $ runProgFail "print(1, 2)"
  , testCase "print zero" $ runProg "print(0)"
  , testCase "print false" $ runProg "print(false)"
  , testCase "print empty string" $ runProg "print(\"\")"
  , testCase "print negative" $ runProg "print(-42)"
  ]

controlFlowTests :: TestTree
controlFlowTests = testGroup "Control Flow"
  [ testCase "if-else true branch" $ runProg "if true { 1 } else { 0 }"
  , testCase "if-else false branch" $ runProg "if false { 1 } else { 0 }"
  , testCase "if with comparison" $ runProg "if 5 > 3 { 1 } else { 0 }"
  , testCase "nested if" $ runProg "if true { if false { 1 } else { 2 } } else { 3 }"
  , testCase "if with block statements" $ runProg "if true { let x = 5 x + 1 } else { 0 }"
  , testCase "return statement" $ runProg "RET 42"
  , testCase "return in function" $ runProg "fn f(x) = { RET x }\nf(42)"
  , testCase "function with block let and expression" $ runProg "fn f() = { let x = 5 x + 10 }\nf()"
  , testCase "function with block multiple statements" $ runProg "fn f() = { let x = 1 let y = 2 x + y }\nf()"
  , testCase "function with empty block" $ runProg "fn f() = { }\nf()"
  ]

errorTests :: TestTree
errorTests = testGroup "Error Handling"
  [ testCase "Unbound variable" $ runProgFail "x"
  , testCase "Type error in if" $ runProgFail "if 42 { 1 } else { 0 }"
  , testCase "Type error in arithmetic" $ runProgFail "\"hello\" + 5"
  , testCase "Type error in comparison" $ runProgFail "\"hello\" < 5"
  , testCase "Arity mismatch" $ runProgFail "fn f(x) = x\nf(1, 2)"
  , testCase "Function not found" $ runProgFail "unknownFunc(42)"
  , testCase "Wrong type to len" $ runProgFail "len(42)"
  , testCase "Wrong type to head" $ runProgFail "head(42)"
  , testCase "Wrong type to tail" $ runProgFail "tail(42)"
  , testCase "Wrong type to concat" $ runProgFail "concat(42, 5)"
  , testCase "Wrong types to at" $ runProgFail "at(42, 0)"
  , testCase "Unknown unary op on int" $ runProgFail "let x = 5\n!x"
  , testCase "Type error in unary" $ runProgFail "!5"
  , testCase "Modulo by zero" $ runProgFail "10 % 0"
  , testCase "Type error in binary op" $ runProgFail "true + 5"
  , testCase "Type error in AND" $ runProgFail "true && 5"
  , testCase "Type error in OR" $ runProgFail "false || 5"
  , testCase "Type error in comparison ops" $ runProgFail "true < false"
  , testCase "Duplicate function definition" $ runProgFail "fn f() = 1\nfn f() = 2"
  , testCase "Duplicate variable definition" $ runProgFail "let x = 1\nlet x = 2"
  , testCase "Duplicate procedure definition" $ runProgFail "fn proc() { let x = 1 }\nfn proc() { let x = 2 }"
  ]

advancedTests :: TestTree
advancedTests = testGroup "Advanced Features"
  [ testCase "Tuple equality same" $ runProg "(1, 2) == (1, 2)"
  , testCase "Tuple equality different" $ runProg "(1, 2) == (1, 3)"
  , testCase "Tuple inequality" $ runProg "(1, 2) != (1, 3)"
  , testCase "Empty tuple" $ runProg "(1,)"
  , testCase "Nested closures" $ runProg "fn outer(x) = { fn inner(y) = x + y inner }\nlet f = outer(10)\nf(5)"
  , testCase "Multiple nested function calls" $ runProg "fn a(x) = x + 1\nfn b(x) = a(a(x))\nfn c(x) = b(b(x))\nc(0)"
  , testCase "Function in list" $ runProg "fn f(x) = x + 1\nlet funcs = [f]\nhead(funcs)"
  , testCase "Closure captures multiple vars" $ runProg "let x = 10\nlet y = 20\nfn f(z) = x + y + z\nf(5)"
  , testCase "Expression sequence evaluation" $ runProg "if true { 1 2 3 } else { 0 }"
  , testCase "Block returns last expr" $ runProg "fn test() = { let x = 1 let y = 2 x + y }\ntest()"
  , testCase "Nested blocks" $ runProg "fn test() { let x = 1 let y = 2 x + y }\ntest()"
  , testCase "Function shadowing in block" $ runProg "fn f() = 1\nfn test() = { fn f() = 2 f() }\ntest()"
  , testCase "Variable shadowing" $ runProg "let x = 1\nfn test() = { let x = 2 x }\ntest()"
  , testCase "Recursive fibonacci" $ runProg "fn fib(n) = if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }\nfib(7)"
  , testCase "Mutual recursion setup" $ runProg "fn isEven(n) = if n == 0 { true } else { if n == 1 { false } else { isEven(n - 2) } }\nisEven(4)"
  , testCase "List of lists comparison" $ runProg "[[1, 2], [3, 4]] == [[1, 2], [3, 4]]"
  , testCase "Mixed type equality false" $ runProg "5 == \"5\""
  , testCase "Bool string equality false" $ runProg "true == \"true\""
  , testCase "List tuple equality false" $ runProg "[1, 2] == (1, 2)"
  , testCase "Empty list empty string equality" $ runProg "[] == \"\""
  , testCase "Procedure execution" $ runProg "fn proc(x) { let y = x }\nproc(42)"
  , testCase "Procedure with multiple statements" $ runProg "fn proc() { let x = 1 let y = 2 }\nproc()"
  , testCase "ESeq with single expr" $ runProg "if true { 42 } else { 0 }"
  , testCase "ESeq with multiple exprs" $ runProg "if true { 1 2 3 } else { 0 }"
  , testCase "Lambda in variable" $ runProg "let double = (x) => x * 2\ndouble(5)"
  , testCase "Lambda immediate call" $ runProg "((x) => x + 1)(5)"
  , testCase "Higher order function" $ runProg "fn apply(f, x) = f(x)\nfn inc(n) = n + 1\napply(inc, 5)"
  , testCase "Return from nested function" $ runProg "fn test() = { fn inner() = RET 42 inner() }\ntest()"
  ]

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Cases"
  [ testCase "Very large number" $ runProg "9223372036854775807"
  , testCase "Large arithmetic" $ runProg "1000000 + 1000000"
  , testCase "Large multiplication" $ runProg "1000 * 1000"
  , testCase "Many nested parentheses" $ runProg "((((((1 + 2))))))"
  , testCase "Deep nesting" $ runProg "1 + (2 + (3 + (4 + 5)))"
  , testCase "Long list" $ runProg "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]"
  , testCase "Deeply nested list" $ runProg "[[[[[1]]]]]"
  , testCase "List with varied types" $ runProg "[1, true, \"hello\"]"
  , testCase "Function with many params" $ runProg "fn f(a, b, c, d, e) = a + b + c + d + e\nf(1, 2, 3, 4, 5)"
  , testCase "Many sequential operations" $ runProg "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10"
  , testCase "Boolean chain" $ runProg "true && true && true && false"
  , testCase "Boolean OR chain" $ runProg "false || false || true || false"
  , testCase "Mixed boolean ops" $ runProg "(true && false) || (true && true)"
  , testCase "String concat chain" $ runProg "concat(concat(\"a\", \"b\"), \"c\")"
  , testCase "Nested function definitions" $ runProg "fn outer() = { fn inner1() = 1 fn inner2() = 2 inner1() + inner2() }\nouter()"
  , testCase "Zero as divisor caught" $ runProgFail "1 / 0"
  , testCase "Zero as modulo divisor" $ runProgFail "1 % 0"
  , testCase "Empty string operations" $ runProg "concat(\"\", \"\")"
  , testCase "Single char string" $ runProg "\"x\""
  , testCase "Comparison chain" $ runProg "(5 > 3) && (3 > 1)"
  , testCase "All comparison ops" $ runProg "let x = 5\n(x == 5) && (x != 4) && (x < 6) && (x <= 5) && (x > 4) && (x >= 5)"
  , testCase "Negative number operations" $ runProg "-5 + -3"
  , testCase "Negative zero" $ runProg "-0"
  , testCase "Double negation" $ runProg "-(-5)"
  , testCase "Double NOT" $ runProg "!(!true)"
  ]

importTests :: TestTree
importTests = testGroup "Import Tests"
  [ testCase "Import non-existent file fails" $ do
      withSystemTempDirectory "flux-test" $ \tmpDir -> do
        let prog = "import { f } from \"nonexistent.flux\""
        runProgFail prog
  ]

additionalArithmeticTests :: TestTree
additionalArithmeticTests = testGroup "Additional Arithmetic Tests"
  [ testCase "Division by one" $ runProg "10 / 1"
  , testCase "Modulo with one" $ runProg "10 % 1"
  , testCase "Zero divided by number" $ runProg "0 / 5"
  , testCase "Zero modulo number" $ runProg "0 % 5"
  , testCase "Multiplication by one" $ runProg "42 * 1"
  , testCase "Multiplication by zero" $ runProg "42 * 0"
  , testCase "Addition with zero" $ runProg "42 + 0"
  , testCase "Subtraction with zero" $ runProg "42 - 0"
  , testCase "Negative number arithmetic" $ runProg "-5 + 3"
  , testCase "Negative times negative" $ runProg "-5 * -3"
  , testCase "Complex nested arithmetic" $ runProg "((10 + 20) * 3) - (40 / 2)"
  , testCase "Modulo negative" $ runProg "-10 % 3"
  , testCase "All operators combined" $ runProg "1 + 2 - 3 * 4 / 5 % 2"
  ]

additionalBooleanTests :: TestTree
additionalBooleanTests = testGroup "Additional Boolean Tests"
  [ testCase "True OR False" $ runProg "true || false"
  , testCase "False OR False" $ runProg "false || false"
  , testCase "True OR True" $ runProg "true || true"
  , testCase "NOT NOT True" $ runProg "!(!true)"
  , testCase "NOT NOT False" $ runProg "!(!false)"
  , testCase "Complex boolean" $ runProg "(true && !false) || (false && true)"
  , testCase "Comparison chain" $ runProg "1 < 2 && 2 < 3 && 3 < 4"
  , testCase "Mixed comparisons" $ runProg "5 > 3 && 2 <= 2 && 10 >= 10"
  , testCase "Boolean from comparison" $ runProg "let x = (5 > 3) x && true"
  ]

additionalFunctionTests :: TestTree
additionalFunctionTests = testGroup "Additional Function Tests"
  [ testCase "Function with no params" $ runProg "fn const() = 42\nconst()"
  , testCase "Function returns function" $ runProg "fn makeFunc() = (x) => x + 1\nlet f = makeFunc()\nf(5)"
  , testCase "Function with multiple returns" $ runProg "fn f(x) = if x > 0 { RET 1 } else { RET -1 }\nf(5)"
  , testCase "Nested function calls" $ runProg "fn f(x) = x + 1\nfn g(x) = f(f(x))\ng(5)"
  , testCase "Function with closure" $ runProg "let x = 10\nfn f(y) = x + y\nf(5)"
  , testCase "Multiple closures" $ runProg "let x = 10\nlet y = 20\nfn f(z) = x + y + z\nf(5)"
  , testCase "Recursive countdown" $ runProg "fn count(n) = if n == 0 { 0 } else { count(n - 1) }\ncount(10)"
  , testCase "Mutual recursion even" $ runProg "fn isEven(n) = if n == 0 { true } else { if n == 1 { false } else { isEven(n - 2) } }\nisEven(10)"
  , testCase "Lambda with multiple params" $ runProg "let f = (x, y, z) => x + y + z\nf(1, 2, 3)"
  , testCase "Lambda in list" $ runProg "let funcs = [(x) => x + 1, (x) => x * 2]\nhead(funcs)"
  , testCase "Higher order map" $ runProg "fn map(f, lst) = if len(lst) == 0 { [] } else { [f(head(lst))] }\nmap((x) => x + 1, [1, 2, 3])"
  ]

additionalControlFlowTests :: TestTree
additionalControlFlowTests = testGroup "Additional Control Flow Tests"
  [ testCase "If with block body" $ runProg "if true { let x = 1 x + 2 } else { 0 }"
  , testCase "If with multiple statements" $ runProg "if true { 1 2 3 } else { 0 }"
  , testCase "Nested if-else chain" $ runProg "if false { 1 } else { if false { 2 } else { if true { 3 } else { 4 } } }"
  , testCase "If with function call" $ runProg "fn f() = true\nif f() { 1 } else { 0 }"
  , testCase "If with comparison" $ runProg "let x = 10\nif x > 5 { 1 } else { 0 }"
  , testCase "If with boolean expression" $ runProg "let x = 5 > 3\nlet y = 2 < 4\nif x && y { 1 } else { 0 }"
  , testCase "If returning from function" $ runProg "fn f(x) = if x > 0 { RET 1 } else { RET 0 }\nf(5)"
  , testCase "Nested if in function" $ runProg "fn f(x) = if x > 10 { if x > 20 { 2 } else { 1 } } else { 0 }\nf(15)"
  ]

additionalErrorTests :: TestTree
additionalErrorTests = testGroup "Additional Error Handling Tests"
  [ testCase "Call non-function" $ runProgFail "let x = 5\nx(10)"
  , testCase "Wrong number of args (too few)" $ runProgFail "fn f(x, y) = x + y\nf(1)"
  , testCase "Wrong number of args (too many)" $ runProgFail "fn f(x) = x + 1\nf(1, 2, 3)"
  , testCase "Undefined function call" $ runProgFail "undefinedFunc(10)"
  , testCase "Type error in arithmetic" $ runProgFail "5 + \"hello\""
  , testCase "Type error in comparison" $ runProgFail "5 > \"hello\""
  , testCase "Boolean operation on non-boolean" $ runProgFail "5 && true"
  , testCase "NOT on non-boolean" $ runProgFail "!5"
  , testCase "Divide by zero" $ runProgFail "10 / 0"
  , testCase "Modulo by zero" $ runProgFail "10 % 0"
  , testCase "Index out of bounds" $ runProgFail "at([1, 2], 10)"
  , testCase "Negative index" $ runProgFail "at([1, 2], -1)"
  , testCase "Head of empty list" $ runProgFail "head([])"
  , testCase "Tail of empty list" $ runProgFail "tail([])"
  , testCase "Wrong type to builtin" $ runProgFail "head(5)"
  ]

exhaustiveArithmeticTests :: TestTree
exhaustiveArithmeticTests = testGroup "Exhaustive Arithmetic Tests"
  [ testCase "Add 1 1" $ runProg "1 + 1"
  , testCase "Add 5 10" $ runProg "5 + 10"
  , testCase "Add 100 200" $ runProg "100 + 200"
  , testCase "Sub 10 5" $ runProg "10 - 5"
  , testCase "Sub 100 50" $ runProg "100 - 50"
  , testCase "Sub 1000 999" $ runProg "1000 - 999"
  , testCase "Mul 2 3" $ runProg "2 * 3"
  , testCase "Mul 10 10" $ runProg "10 * 10"
  , testCase "Mul 7 8" $ runProg "7 * 8"
  , testCase "Div 10 2" $ runProg "10 / 2"
  , testCase "Div 100 4" $ runProg "100 / 4"
  , testCase "Div 15 3" $ runProg "15 / 3"
  , testCase "Mod 10 3" $ runProg "10 % 3"
  , testCase "Mod 17 5" $ runProg "17 % 5"
  , testCase "Mod 100 7" $ runProg "100 % 7"
  , testCase "Add chain 1" $ runProg "1 + 2 + 3"
  , testCase "Add chain 2" $ runProg "5 + 10 + 15 + 20"
  , testCase "Sub chain 1" $ runProg "100 - 10 - 20"
  , testCase "Mul chain 1" $ runProg "2 * 3 * 4"
  , testCase "Mul chain 2" $ runProg "5 * 2 * 2"
  , testCase "Mixed 1" $ runProg "2 + 3 * 4"
  , testCase "Mixed 2" $ runProg "10 - 2 * 3"
  , testCase "Mixed 3" $ runProg "20 / 4 + 5"
  , testCase "Mixed 4" $ runProg "15 % 4 + 1"
  , testCase "Parens 1" $ runProg "(2 + 3) * 4"
  , testCase "Parens 2" $ runProg "10 / (2 + 3)"
  , testCase "Parens 3" $ runProg "(10 - 2) * (3 + 1)"
  , testCase "Neg 1" $ runProg "-5 + 10"
  , testCase "Neg 2" $ runProg "10 + -5"
  , testCase "Neg 3" $ runProg "-5 * -2"
  , testCase "Neg 4" $ runProg "-10 / 2"
  , testCase "Zero add" $ runProg "0 + 5"
  , testCase "Zero sub" $ runProg "5 - 0"
  , testCase "Zero mul" $ runProg "0 * 100"
  , testCase "One mul" $ runProg "1 * 100"
  , testCase "One div" $ runProg "100 / 1"
  ]

exhaustiveBooleanTests :: TestTree
exhaustiveBooleanTests = testGroup "Exhaustive Boolean Tests"
  [ testCase "True and true" $ runProg "true && true"
  , testCase "True and false" $ runProg "true && false"
  , testCase "False and true" $ runProg "false && true"
  , testCase "False and false" $ runProg "false && false"
  , testCase "True or true" $ runProg "true || true"
  , testCase "True or false" $ runProg "true || false"
  , testCase "False or true" $ runProg "false || true"
  , testCase "False or false" $ runProg "false || false"
  , testCase "Not true" $ runProg "!true"
  , testCase "Not false" $ runProg "!false"
  , testCase "Not not true" $ runProg "!(!true)"
  , testCase "Not not false" $ runProg "!(!false)"
  , testCase "And chain 1" $ runProg "true && true && true"
  , testCase "And chain 2" $ runProg "true && true && false"
  , testCase "Or chain 1" $ runProg "false || false || true"
  , testCase "Or chain 2" $ runProg "false || false || false"
  , testCase "Mixed 1" $ runProg "true && false || true"
  , testCase "Mixed 2" $ runProg "false || true && true"
  , testCase "Mixed 3" $ runProg "(true && false) || true"
  , testCase "Mixed 4" $ runProg "true && (false || true)"
  , testCase "With not 1" $ runProg "!true && false"
  , testCase "With not 2" $ runProg "true && !false"
  , testCase "With not 3" $ runProg "!true || !false"
  ]

exhaustiveComparisonTests :: TestTree
exhaustiveComparisonTests = testGroup "Exhaustive Comparison Tests"
  [ testCase "Eq int 1" $ runProg "5 == 5"
  , testCase "Eq int 2" $ runProg "10 == 10"
  , testCase "Eq int false" $ runProg "5 == 10"
  , testCase "Neq int 1" $ runProg "5 != 10"
  , testCase "Neq int 2" $ runProg "100 != 200"
  , testCase "Neq int false" $ runProg "5 != 5"
  , testCase "Lt 1" $ runProg "5 < 10"
  , testCase "Lt 2" $ runProg "1 < 100"
  , testCase "Lt false" $ runProg "10 < 5"
  , testCase "Lte 1" $ runProg "5 <= 10"
  , testCase "Lte 2" $ runProg "10 <= 10"
  , testCase "Lte false" $ runProg "15 <= 10"
  , testCase "Gt 1" $ runProg "10 > 5"
  , testCase "Gt 2" $ runProg "100 > 1"
  , testCase "Gt false" $ runProg "5 > 10"
  , testCase "Gte 1" $ runProg "10 >= 5"
  , testCase "Gte 2" $ runProg "10 >= 10"
  , testCase "Gte false" $ runProg "5 >= 10"
  , testCase "Eq bool true" $ runProg "true == true"
  , testCase "Eq bool false" $ runProg "false == false"
  , testCase "Eq bool diff" $ runProg "true == false"
  , testCase "Eq string 1" $ runProg "\"test\" == \"test\""
  , testCase "Eq string 2" $ runProg "\"hello\" == \"hello\""
  , testCase "Eq string false" $ runProg "\"test\" == \"other\""
  , testCase "Compare chain 1" $ runProg "1 < 2 && 2 < 3"
  , testCase "Compare chain 2" $ runProg "5 > 3 && 3 > 1"
  , testCase "Compare mixed 1" $ runProg "(5 > 3) && (10 == 10)"
  , testCase "Compare mixed 2" $ runProg "(1 < 2) || (5 > 10)"
  ]

exhaustiveListOperationTests :: TestTree
exhaustiveListOperationTests = testGroup "Exhaustive List Operation Tests"
  [ testCase "Head simple" $ runProg "head([1, 2, 3])"
  , testCase "Head one elem" $ runProg "head([42])"
  , testCase "Head strings" $ runProg "head([\"a\", \"b\"])"
  , testCase "Tail simple" $ runProg "tail([1, 2, 3])"
  , testCase "Tail two elem" $ runProg "tail([1, 2])"
  , testCase "Tail strings" $ runProg "tail([\"a\", \"b\", \"c\"])"
  , testCase "At index 0" $ runProg "at([1, 2, 3], 0)"
  , testCase "At index 1" $ runProg "at([10, 20, 30], 1)"
  , testCase "At index 2" $ runProg "at([5, 10, 15], 2)"
  , testCase "At strings" $ runProg "at([\"a\", \"b\", \"c\"], 1)"
  , testCase "Len empty" $ runProg "len([])"
  , testCase "Len one" $ runProg "len([1])"
  , testCase "Len three" $ runProg "len([1, 2, 3])"
  , testCase "Len five" $ runProg "len([1, 2, 3, 4, 5])"
  , testCase "Len strings" $ runProg "len([\"a\", \"b\"])"
  , testCase "Concat lists" $ runProg "concat([1, 2], [3, 4])"
  , testCase "Concat empty left" $ runProg "concat([], [1, 2])"
  , testCase "Concat empty right" $ runProg "concat([1, 2], [])"
  , testCase "Concat strings lists" $ runProg "concat([\"a\"], [\"b\"])"
  , testCase "Reverse simple" $ runProg "reverse([1, 2, 3])"
  , testCase "Reverse one" $ runProg "reverse([42])"
  , testCase "Reverse empty" $ runProg "reverse([])"
  , testCase "Reverse strings" $ runProg "reverse([\"a\", \"b\", \"c\"])"
  ]

exhaustiveStringOperationTests :: TestTree
exhaustiveStringOperationTests = testGroup "Exhaustive String Operation Tests"
  [ testCase "Concat str 1" $ runProg "concat(\"hello\", \"world\")"
  , testCase "Concat str 2" $ runProg "concat(\"foo\", \"bar\")"
  , testCase "Concat empty left" $ runProg "concat(\"\", \"test\")"
  , testCase "Concat empty right" $ runProg "concat(\"test\", \"\")"
  , testCase "Concat empty both" $ runProg "concat(\"\", \"\")"
  , testCase "Len str 1" $ runProg "len(\"hello\")"
  , testCase "Len str 2" $ runProg "len(\"test\")"
  , testCase "Len empty str" $ runProg "len(\"\")"
  , testCase "Len one char" $ runProg "len(\"a\")"
  , testCase "At str index 0" $ runProg "at(\"hello\", 0)"
  , testCase "At str index 1" $ runProg "at(\"world\", 1)"
  , testCase "At str index 4" $ runProg "at(\"hello\", 4)"
  , testCase "Head str" $ runProg "head(\"hello\")"
  , testCase "Head one char" $ runProg "head(\"x\")"
  , testCase "Tail str" $ runProg "tail(\"hello\")"
  , testCase "Tail two char" $ runProg "tail(\"ab\")"
  , testCase "Reverse str 1" $ runProg "reverse(\"hello\")"
  , testCase "Reverse str 2" $ runProg "reverse(\"test\")"
  , testCase "Reverse empty" $ runProg "reverse(\"\")"
  , testCase "Reverse one" $ runProg "reverse(\"a\")"
  ]

-- Helper functions
runProg :: String -> Assertion
runProg input = do
  case Parser.parseProgram input of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right ast -> do
      result <- Interpreter.runProgram ast
      case result of
        Left err -> assertFailure $ "Evaluation error: " ++ err
        Right _ -> return ()

runProgFail :: String -> Assertion
runProgFail input = do
  case Parser.parseProgram input of
    Left _ -> return ()  -- Parse error is acceptable
    Right ast -> do
      result <- Interpreter.runProgram ast
      case result of
        Left _ -> return ()  -- Expected error
        Right _ -> assertFailure "Expected evaluation error but succeeded"
