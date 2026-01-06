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

Each line is a new instruction like Python but not Identation base. For instance Condition or Loop Condition it will be more like C++ with brackets and also condition is without the `()`:

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

### 4. Loop Constructs

**While loop (with mutable variable reassignment):**
```
fn countDown(n) {
    let mut i = n
    while i > 0 {
        print(i)
        i = i - 1
    }
}
```

**Alternative while syntax (without `mut` keyword, implicit mutability inside loops):**
```
fn sumToN(n) = {
    let sum = 0
    let i = 1
    while i <= n {
        sum = sum + i
        i = i + 1
    }
    RET sum
}
```

**For loop (range-based with `..` operator):**
```
fn printRange(start, end) {
    for i in start..end {
        print(i)
    }
}

// Or inclusive range with `..=`
fn printRangeInclusive(start, end) {
    for i in start..=end {
        print(i)
    }
}
```

**For loop with step:**
```
fn printEvens(start, end) {
    for i in start..end step 2 {
        print(i)
    }
}
```

**List iteration:**
```
fn printAll(items) {
    for item in items {
        print(item)
    }
}

// With index
fn printWithIndex(items) {
    for (i, item) in enumerate(items) {
        print(i)
        print(item)
    }
}
```

**Loop control:**
```
fn findFirst(items, target) = {
    for item in items {
        if item == target {
            RET item  // early return from function
        }
    }
    RET -1  // not found
}

// break and continue for loop control
fn sumEvens(items) = {
    let sum = 0
    for item in items {
        if item < 0 {
            break  // exit loop entirely
        }
        if item % 2 != 0 {
            continue  // skip to next iteration
        }
        sum = sum + item
    }
    RET sum
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
