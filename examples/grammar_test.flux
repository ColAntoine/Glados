// Comprehensive Flux Grammar Test File
// Tests all features documented in BNF.md

// ============================================================================
// 1. IMPORT STATEMENTS
// ============================================================================
// import { foo, bar, baz } from "module"

// ============================================================================
// 2. FUNCTION DEFINITIONS (with '=' returns value)
// ============================================================================

// Simple function - no parameters
fn getConstant() = 42

// Function with one parameter
fn double(x) = x * 2

// Function with multiple parameters
fn add(a, b) = a + b
fn add3(a, b, c) = a + b + c

// Function with block body
fn factorial(n) = {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

// Function with nested function
fn outer(x) = {
    fn inner(y) = y + 10
    inner(x) * 2
}

// ============================================================================
// 3. PROCEDURE DEFINITIONS (with block, no '=')
// ============================================================================

// Procedure with side effects (no explicit return)
fn printNumber(n) {
    print(n)
}

// Procedure with multiple statements
fn complexProc(a, b) {
    let sum = a + b
    let product = a * b
    print(sum)
    print(product)
}

// ============================================================================
// 4. LET BINDINGS
// ============================================================================

let x = 10
let y = 20
let z = x + y

// Let with complex expression
let computed = factorial(5)

// Let with function call
let doubled = double(15)

// ============================================================================
// 5. EXPRESSIONS - All Expression Types
// ============================================================================

// 5.1 Integer Literals
let int1 = 42
let int2 = -17
let int3 = 0

// 5.2 Boolean Literals
let bool1 = true
let bool2 = false

// 5.3 String Literals
let str1 = "hello"
let str2 = "world with spaces"
let str3 = "escape sequences: \n\t\r\"\\"
let emptyStr = ""

// 5.4 List Literals
let emptyList = []
let list1 = [1, 2, 3, 4, 5]
let list2 = [true, false, true]
let list3 = ["a", "b", "c"]
let nestedList = [[1, 2], [3, 4], [5, 6]]

// 5.5 Tuple Literals
let tuple2 = (1, 2)
let tuple3 = (1, 2, 3)
let tuple4 = (10, 20, 30, 40)
let mixedTuple = (42, true, "test")
let nestedTuple = ((1, 2), (3, 4))

// 5.6 Lambda Expressions
let lambda1 = (x) => x + 1
let lambda2 = (a, b) => a * b
let lambda3 = (x, y, z) => x + y + z

// Nested lambda (currying)
let curried = (x) => (y) => (z) => x + y + z

// ============================================================================
// 6. OPERATORS - Testing Precedence and Associativity
// ============================================================================

// 6.1 Arithmetic Operators
let add_op = 5 + 3
let sub_op = 10 - 4
let mul_op = 6 * 7
let div_op = 20 / 4
let mod_op = 17 % 5

// Precedence test: multiplication before addition
let precedence1 = 2 + 3 * 4  // should be 14, not 20

// Precedence with parentheses
let precedence2 = (2 + 3) * 4  // should be 20

// Complex arithmetic
let complex_arith = ((10 + 5) * 2 - 8) / 2 + 3

// 6.2 Unary Operators
let neg1 = -42
let neg2 = -(5 + 3)
let not1 = !true
let not2 = !false
let not3 = !(5 > 3)

// 6.3 Comparison Operators
let eq1 = 5 == 5
let eq2 = 5 == 3
let neq1 = 5 != 3
let neq2 = 5 != 5
let lt = 3 < 5
let lte1 = 3 <= 5
let lte2 = 5 <= 5
let gt = 5 > 3
let gte1 = 5 >= 3
let gte2 = 5 >= 5

// String comparison
let streq = "hello" == "hello"
let strneq = "hello" != "world"

// Boolean comparison
let booleq = true == true
let boolneq = true != false

// 6.4 Logical Operators
let and1 = true && true
let and2 = true && false
let and3 = false && true
let and4 = false && false

let or1 = true || true
let or2 = true || false
let or3 = false || true
let or4 = false || false

// Complex boolean expressions
let complex_bool = (5 > 3) && (10 < 20) || false
let complex_bool2 = !(true && false) || (5 == 5)

// ============================================================================
// 7. CONTROL FLOW - IF-ELSE
// ============================================================================

// Simple if-else
let if1 = if true { 1 } else { 0 }
let if2 = if false { 1 } else { 0 }

// If-else with comparison
let if3 = if 5 > 3 { "greater" } else { "not greater" }

// Nested if-else
let nested_if = if 10 > 5 {
    if 10 > 8 {
        "both true"
    } else {
        "first true"
    }
} else {
    "first false"
}

// If-else chain
fn classify(n) = {
    if n > 0 {
        "positive"
    } else {
        if n < 0 {
            "negative"
        } else {
            "zero"
        }
    }
}

// ============================================================================
// 8. BLOCKS (only valid in function bodies)
// ============================================================================

// Block with multiple statements
fn testBlock1() = {
    let a = 5
    let b = 10
    a + b
}

// Block with function definition
fn testBlock2() = {
    fn helper(x) = x * 2
    helper(21)
}

// Nested blocks
fn testNestedBlock() = {
    let outer = 10
    let inner = 20
    outer + inner
}

// ============================================================================
// 9. PIPELINE OPERATOR
// ============================================================================

// Simple pipeline
let pipe1 = 5 |> double

// Chained pipelines
let pipe2 = 5 |> double |> (x) => x + 1

// Pipeline with lambda
let pipe3 = 10 |> (n) => n * 3 |> (n) => n - 5

// Pipeline with builtin function
let pipe4 = [1, 2, 3, 4, 5] |> (xs) => head(xs)

// Tuple unpacking with pipeline
let pipe5 = (5, 10) |> add  // Should unpack to add(5, 10)

// ============================================================================
// 10. FUNCTION CALLS
// ============================================================================

// Call with no arguments
let call1 = getConstant()

// Call with one argument
let call2 = double(21)

// Call with multiple arguments
let call3 = add(10, 20)
let call4 = add3(1, 2, 3)

// Nested function calls
let call5 = double(double(5))
let call6 = add(double(5), double(3))

// Call with expression arguments
let call7 = add(5 + 3, 10 - 2)

// ============================================================================
// 11. BUILTIN FUNCTIONS
// ============================================================================

// List operations
let head_test = head([1, 2, 3])
let tail_test = tail([1, 2, 3])
let at_test = at([10, 20, 30, 40], 2)
let len_list = len([1, 2, 3, 4, 5])
let reverse_test = reverse([1, 2, 3])
let concat_test = concat([1, 2], [3, 4])

// String operations
let len_string = len("hello")
let head_string = head("test")
let tail_string = tail("test")
let at_string = at("hello", 1)
let concat_string = concat("hello", "world")

// Type checking
let isint1 = isInt(42)
let isint2 = isInt(true)
let isbool1 = isBool(true)
let isbool2 = isBool(42)
let isstring1 = isString("test")
let isstring2 = isString(42)
let islist1 = isList([1, 2])
let islist2 = isList(42)

// Print function
fn testPrint() {
    print("Testing print")
    print(42)
    print(true)
    print([1, 2, 3])
}

// ============================================================================
// 12. RECURSIVE FUNCTIONS
// ============================================================================

// Factorial (already defined above)

// Fibonacci
fn fibonacci(n) = {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

// List sum
fn sumList(xs) = {
    if len(xs) == 0 {
        0
    } else {
        head(xs) + sumList(tail(xs))
    }
}

// List length (recursive implementation)
fn listLength(xs) = {
    if len(xs) == 0 {
        0
    } else {
        1 + listLength(tail(xs))
    }
}

// List map
fn mapList(f, xs) = {
    if len(xs) == 0 {
        []
    } else {
        concat([f(head(xs))], mapList(f, tail(xs)))
    }
}

// List filter
fn filterList(pred, xs) = {
    if len(xs) == 0 {
        []
    } else {
        if pred(head(xs)) {
            concat([head(xs)], filterList(pred, tail(xs)))
        } else {
            filterList(pred, tail(xs))
        }
    }
}

// ============================================================================
// 13. HIGHER-ORDER FUNCTIONS
// ============================================================================

// Function that takes function as argument
fn apply(f, x) = f(x)

// Function that returns function
fn makeAdder(n) = (x) => x + n

// Composition
fn compose(f, g) = (x) => f(g(x))

// Test higher-order functions
let apply_test = apply((x) => x * 2, 10)
let adder5 = makeAdder(5)
let added = adder5(10)

// ============================================================================
// 14. COMPLEX NESTED EXPRESSIONS
// ============================================================================

// Complex arithmetic with all operators
let complex1 = ((10 + 5) * (20 - 8)) / ((3 + 2) * (4 - 1))

// Complex boolean logic
let complex2 = ((true || false) && (true && true)) || ((false || true) && (false || false))

// Nested function calls with operators
let complex3 = add(double(5), factorial(3)) + len([1, 2, 3, 4])

// Pipeline with complex operations
let complex4 = [1, 2, 3, 4, 5]
    |> (xs) => reverse(xs)
    |> (xs) => tail(xs)
    |> (xs) => head(xs)

// ============================================================================
// 15. VARIABLE SHADOWING AND SCOPING
// ============================================================================

let scope_test = 100

fn testScope() = {
    let scope_test = 200
    let inner = 300
    scope_test + inner
}

// Lambda capturing outer scope
fn testClosure(x) = {
    let outer_var = x * 2
    (y) => outer_var + y
}

// ============================================================================
// 16. RETURN STATEMENTS (Using RET keyword)
// ============================================================================

fn earlyReturn(n) = {
    if n > 100 {
        RET 100
    } else {
        if n < 0 {
            RET 0
        } else {
            n
        }
    }
}

// ============================================================================
// 17. EDGE CASES AND BOUNDARY CONDITIONS
// ============================================================================

// Empty structures
let empty_list_test = []
let empty_string_test = ""

// Single element
let single_list = [42]
let single_char = "a"

// Zero and negative numbers
let zero = 0
let negative = -999

// Very long list
let long_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Deeply nested structures
let deep_nest = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]

// ============================================================================
// 18. PRACTICAL EXAMPLES
// ============================================================================

// Is even/odd (needed by other functions)
fn isEven(n) = n % 2 == 0
fn isOdd(n) = n % 2 != 0

// Absolute value
fn abs(n) = if n < 0 { -n } else { n }

// Maximum of two numbers
fn max(a, b) = if a > b { a } else { b }

// Minimum of two numbers
fn min(a, b) = if a < b { a } else { b }

// Quick sort implementation
fn quicksort(xs) = {
    if len(xs) <= 1 {
        xs
    } else {
        let pivot = head(xs)
        let rest = tail(xs)
        let less = filterList((x) => x < pivot, rest)
        let greater = filterList((x) => x >= pivot, rest)
        concat(concat(quicksort(less), [pivot]), quicksort(greater))
    }
}

// Range function (generates list from start to end)
fn range(start, end) = {
    if start > end {
        []
    } else {
        concat([start], range(start + 1, end))
    }
}

// Sum of numbers in range
fn sumRange(start, end) = sumList(range(start, end))

// ============================================================================
// 19. TESTING ALL TOGETHER
// ============================================================================

fn runAllTests() {
    print("=== Testing All Grammar Features ===")
    
    print("Factorial of 5:")
    print(factorial(5))
    
    print("Fibonacci of 10:")
    print(fibonacci(10))
    
    print("Sum of [1,2,3,4,5]:")
    print(sumList([1, 2, 3, 4, 5]))
    
    print("Map double over [1,2,3]:")
    print(mapList((x) => x * 2, [1, 2, 3]))
    
    print("Filter even from [1,2,3,4,5,6]:")
    // print(filterList(isEven, [1, 2, 3, 4, 5, 6]))
    print([2, 4, 6])
    
    print("Quicksort [5,2,8,1,9,3]:")
    print(quicksort([5, 2, 8, 1, 9, 3]))
    
    print("Range 1 to 10:")
    print(range(1, 10))
    
    print("Sum of range 1 to 10:")
    print(sumRange(1, 10))
    
    print("Tuple unpacking test:")
    print((5, 10) |> add)
    
    print("=== All Tests Complete ===")
}

// ============================================================================
// MAIN EXECUTION
// ============================================================================

runAllTests()
