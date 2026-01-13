# Flux Syntax Specification

## Core Syntax

### Functions

**function with no return (doesn't store value):**
```
fn Multiplication(Param1, Param2) {
    let result = Param1 * Param2
    print(result)
}
```

**function with return (store value):**
```
fn Func(Param1, Param2) = {
    let result = Param1 * Param2
    result + 1
}

(8, 10) |> Func |> print   or    Func(8, 10) |> print
```

**let** used to define a variable (needs to detect where String/Int/Bool...)

The function that returns should always have the `=` with the `{}` and it returns or stores the value of the last line of the function. (can't be an output tho like print or else its an error, should be a value Int/String/Bool ...etc)

**One liner too can be possible like so:**
```
fn Multiplication(Param1, Param2) = Param1 * Param2
(8, 10) |> Multiplication |> print
```

Each line is a new instruction like Python but not indentation based. Conditions use curly brackets (like C++) but without parentheses around the condition:

```
//example with string
fn ConditionStr(n) = {
    if x % 2 == 0 {
        RET "is even"
    } else {
        RET "is odd"
    }
}
4 |> ConditionStr |> print

//example with Bool
fn ConditionBool(n) = {
    if x % 2 == 0 {
        RET True
    } else {
        RET Even
    }
}

5 |> ConditionBool |> print
```

---

## Additional Syntax Rules & Enhancements

### 1. If/Else Termination & Control Flow

**Problem:** How do we distinguish between:
- Returning a value from an if/else (in a returning function)
- Executing side-effects in if/else (in a procedure)

**Solution:** Use implicit return for the last expression in each branch. If you want to execute side-effects without returning, add explicit statements:

```
// Returning function - last expression in each branch is the return value
fn classify(n) = {
    if n > 0 {
        "positive"
    } else {
        "negative or zero"
    }
}

// Procedure - if/else can contain statements, no implicit return
fn logClassify(n) {
    if n > 0 {
        print("positive")
    } else {
        print("negative or zero")
    }
}

// Mixed: returning function with side-effects before return
fn classifyAndLog(n) = {
    let category = if n > 0 { "positive" } else { "negative" }
    print(category)
    category  // last line is the return value
}
```

**Alternative approach (explicit `RET` keyword):**
If you want explicit control, add a `RET` keyword:
```
fn classify(n) = {
    if n > 0 {
        RET "positive"
    } else {
        RET "negative"
    }
}
```
Without `RET`, the last expression is returned. With `RET`, you can exit early from anywhere in the function.

### 2. Tuple Syntax & Unpacking

**Tuple literals:**
```
let pair = (10, 20)
let triple = (1, 2, 3)
```

**Tuple unpacking in function calls:**
```
fn add(a, b) = a + b

(5, 10) |> add |> print  // unpacks tuple to add(5, 10)
```

**Tuple unpacking in let:**
```
let (x, y) = (10, 20)
print(x)  // prints 10
```

### 3. Native String Type

**String literals:**
```
let greeting = "Hello, World!"
print(greeting)
```

**String operations (builtins to add):**
- `len(str)` - length
- `concat(str1, str2)` - concatenation
- `charAt(str, index)` - get character at index
- `substring(str, start, end)` - extract substring

**String in conditions:**
```
fn greet(name) = {
    if name == "Alice" {
        "Hello, Alice!"
    } else {
        "Hello, stranger!"
    }
}
```

### 4. Recursion (Iteration in Functional Style)

Flux is a functional language and uses **recursion** instead of loops. Recursion is the primary mechanism for iteration and repetitive operations.

**Basic recursion - Countdown:**
```flux
fn countDown(n) = {
    if n > 0 {
        print(n)
        countDown(n - 1)
    } else {
        0  // base case
    }
}

countDown(5)  // prints 5, 4, 3, 2, 1
```

**Accumulator pattern - Sum to N:**
```flux
fn sumToN(n) = sumHelper(n, 0, 1)

fn sumHelper(n, sum, i) = {
    if i <= n {
        sumHelper(n, sum + i, i + 1)
    } else {
        sum
    }
}

sumToN(10) |> print  // prints 55
```

**Recursive factorial:**
```flux
fn fact(n) = {
    if n <= 1 {
        1
    } else {
        n * fact(n - 1)
    }
}

fact(10) |> print  // prints 3628800
```

**Recursive Fibonacci:**
```flux
fn fib(n) = {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fib(10) |> print  // prints 55
```

**List recursion - Processing lists:**
```flux
fn sumList(list) = {
    if len(list) == 0 {
        0
    } else {
        head(list) + sumList(tail(list))
    }
}

fn printList(list, index) = {
    if index < len(list) {
        print(at(list, index))
        printList(list, index + 1)
    } else {
        0
    }
}

printList([1, 2, 3, 4, 5], 0)
```

**Finding in a list recursively:**
```flux
fn findInList(list, target, index) = {
    if index >= len(list) {
        -1  // not found
    } else {
        if at(list, index) == target {
            index
        } else {
            findInList(list, target, index + 1)
        }
    }
}

findInList([10, 20, 30], 20, 0) |> print  // prints 1
```

**Tail recursion optimization:**
When a recursive call is the last operation in a function (tail position), it can be optimized by the compiler to avoid stack overflow:
```flux
// Tail-recursive factorial with accumulator
fn factTail(n) = factHelper(n, 1)

fn factHelper(n, acc) = {
    if n <= 1 {
        acc
    } else {
        factHelper(n - 1, n * acc)  // tail position
    }
}
```

### 5. Validation Rules

**Returning functions:**
- Must have `=` after parameters
- Last expression must evaluate to a value (not a statement like `print`)
- Error if last line is a side-effect without returning a value

**Procedures:**
- No `=` after parameters
- Can contain side-effects (print, mutations, etc.)
- Don't return values (or return unit/void)

**Example violations:**
```
// ERROR: returning function ends with print (side-effect, no value)
fn bad(n) = {
    let x = n + 1
    print(x)  // error: print doesn't return a value
}

// CORRECT: return the value after printing
fn good(n) = {
    let x = n + 1
    print(x)
    x  // last line is the return value
}
```

### 6. Comments

Single-line comments with `//`:
```
// This is a comment
fn add(a, b) = a + b  // inline comment
```

Multi-line comments (optional):
```
/*
  This is a
  multi-line comment
*/
fn multiply(a, b) = a * b
```

### 7. Operator Precedence (current)

1. Unary: `!`, `-`
2. Multiplicative: `*`, `/`, `%`
3. Additive: `+`, `-`
4. Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
5. Logical AND: `&&`
6. Logical OR: `||`
7. Pipeline: `|>`

### 8. Type System (future consideration)

Currently dynamically typed. For future:
- Optional type annotations: `fn add(a: Int, b: Int) -> Int = a + b`
- Type inference for most cases
- Runtime type checks with clear error messages

### 9. Syntactic Sugar (planned feature)

Flux can support convenient shorthand operators that desugar to their expanded forms during parsing.

**Assignment operators (planned):**
- `x += y` → `x = x + y` (addition assignment)
- `x -= y` → `x = x - y` (subtraction assignment)
- `x *= y` → `x = x * y` (multiplication assignment)
- `x /= y` → `x = x / y` (division assignment)
- `x %= y` → `x = x % y` (modulo assignment)

**Increment/Decrement (planned):**
- `x++` → `x = x + 1` (increment)
- `x--` → `x = x - 1` (decrement)

**Example usage (when implemented):**
```flux
fn factorial(n, acc) = {
    if n <= 1 {
        acc
    } else {
        acc *= n  // Instead of: let acc = acc * n
        factorial(n - 1, acc)
    }
}
```

**Current workaround:**
Until these operators are implemented, use the expanded form:
```flux
fn factorial(n, acc) = {
    if n <= 1 {
        acc
    } else {
        let acc *= n  // Expanded form
        factorial(n - 1, acc)
    }
}

