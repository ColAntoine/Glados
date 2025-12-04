# Flux AST Documentation

This document describes the Abstract Syntax Tree (AST) structure of the Flux language, with examples of how source code maps to AST nodes. This is essential reference material for implementing a bytecode compiler.

## Table of Contents
- [AST Type Definitions](#ast-type-definitions)
- [Top-Level Forms](#top-level-forms)
- [Expression Nodes](#expression-nodes)
- [Pipeline Desugaring](#pipeline-desugaring)
- [Complete Examples](#complete-examples)

---

## AST Type Definitions

The Flux AST is defined in Haskell (`FLUX/src/AST.hs`):

```haskell
type Program = [TopLevel]

data TopLevel
    = TLFn String [String] Expr      -- Function definition
    | TLProc String [String] [TopLevel]  -- Procedure (no return value)
    | TLLet String Expr              -- Top-level binding
    | TLExpr Expr                    -- Top-level expression

data Expr
    = EInt Int64                     -- Integer literal
    | EBool Bool                     -- Boolean literal
    | EString String                 -- String literal (native)
    | EVar String                    -- Variable reference
    | EList [Expr]                   -- List literal
    | ETuple [Expr]                  -- Tuple literal
    | ELam [String] Expr             -- Lambda/anonymous function
    | EIf Expr Expr Expr             -- Conditional (cond, then, else)
    | ECall Expr [Expr]              -- Function call
    | EUnary String Expr             -- Unary operator
    | EBinary Op Expr Expr           -- Binary operator
    | EBlock [TopLevel] (Maybe Expr) -- Block with local definitions
    | ERet Expr                      -- Explicit return

data Op = Add | Sub | Mul | Div | Mod
        | Eq | Neq | Lt | Lte | Gt | Gte
        | And | Or | Pipe
```

---

## Top-Level Forms

### 1. Function Definition (`TLFn`)

**Syntax:**
```flux
fn name(param1, param2) = expression
fn name(param1, param2) = { block }
```

**Example:**
```flux
fn add(a, b) = a + b
```

**Parsed AST:**
```haskell
TLFn "add" ["a","b"] (EBinary Add (EVar "a") (EVar "b"))
```

**With block body:**
```flux
fn factorial(n) = {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}
```

**Parsed AST:**
```haskell
TLFn "factorial" ["n"] 
    (EBlock [] 
        (Just (EIf 
            (EBinary Lte (EVar "n") (EInt 1))
            (EInt 1)
            (EBinary Mul (EVar "n") 
                (ECall (EVar "factorial") [EBinary Sub (EVar "n") (EInt 1)])))))
```

### 2. Procedure Definition (`TLProc`)

Procedures don't return values (no `=` after parameters):

**Syntax:**
```flux
fn name(params) {
    statement1
    statement2
}
```

**Example:**
```flux
fn greet(name) {
    print("Hello")
    print(name)
}
```

**Parsed AST:**
```haskell
TLProc "greet" ["name"] 
    [TLExpr (ECall (EVar "print") [EString "Hello"]),
     TLExpr (ECall (EVar "print") [EVar "name"])]
```

### 3. Top-Level Let Binding (`TLLet`)

**Syntax:**
```flux
let name = expression
```

**Example:**
```flux
let x = 42
```

**Parsed AST:**
```haskell
TLLet "x" (EInt 42)
```

### 4. Top-Level Expression (`TLExpr`)

**Example:**
```flux
print(42)
```

**Parsed AST:**
```haskell
TLExpr (ECall (EVar "print") [EInt 42])
```

---

## Expression Nodes

### Literals

| Flux Code | AST Node |
|-----------|----------|
| `42` | `EInt 42` |
| `true` | `EBool True` |
| `false` | `EBool False` |
| `"hello"` | `EString "hello"` |
| `[1, 2, 3]` | `EList [EInt 1, EInt 2, EInt 3]` |
| `(5, 10)` | `ETuple [EInt 5, EInt 10]` |

### Variable Reference

```flux
x
```
→ `EVar "x"`

### Function Call

**Direct call:**
```flux
add(5, 10)
```
→ `ECall (EVar "add") [EInt 5, EInt 10]`

**Call with expression as function:**
```flux
(fn(x) = x + 1)(5)
```
→ `ECall (ELam ["x"] (EBinary Add (EVar "x") (EInt 1))) [EInt 5]`

### Lambda

```flux
fn(x, y) = x + y
```
→ `ELam ["x","y"] (EBinary Add (EVar "x") (EVar "y"))`

### Conditional

```flux
if x > 0 {
    "positive"
} else {
    "non-positive"
}
```
→ `EIf (EBinary Gt (EVar "x") (EInt 0)) (EString "positive") (EString "non-positive")`

### Binary Operators

| Operator | Op Type | Example | AST |
|----------|---------|---------|-----|
| `+` | `Add` | `a + b` | `EBinary Add (EVar "a") (EVar "b")` |
| `-` | `Sub` | `a - b` | `EBinary Sub (EVar "a") (EVar "b")` |
| `*` | `Mul` | `a * b` | `EBinary Mul (EVar "a") (EVar "b")` |
| `/` | `Div` | `a / b` | `EBinary Div (EVar "a") (EVar "b")` |
| `%` | `Mod` | `a % b` | `EBinary Mod (EVar "a") (EVar "b")` |
| `==` | `Eq` | `a == b` | `EBinary Eq (EVar "a") (EVar "b")` |
| `!=` | `Neq` | `a != b` | `EBinary Neq (EVar "a") (EVar "b")` |
| `<` | `Lt` | `a < b` | `EBinary Lt (EVar "a") (EVar "b")` |
| `<=` | `Lte` | `a <= b` | `EBinary Lte (EVar "a") (EVar "b")` |
| `>` | `Gt` | `a > b` | `EBinary Gt (EVar "a") (EVar "b")` |
| `>=` | `Gte` | `a >= b` | `EBinary Gte (EVar "a") (EVar "b")` |
| `&&` | `And` | `a && b` | `EBinary And (EVar "a") (EVar "b")` |
| `\|\|` | `Or` | `a \|\| b` | `EBinary Or (EVar "a") (EVar "b")` |
| `\|>` | `Pipe` | `a \|> b` | `EBinary Pipe (EVar "a") (EVar "b")` (before desugaring) |

### Unary Operators

| Operator | Example | AST |
|----------|---------|-----|
| `-` | `-x` | `EUnary "-" (EVar "x")` |
| `!` | `!x` | `EUnary "!" (EVar "x")` |

### Block with Local Definitions

```flux
{
    let x = 10
    fn double(n) = n * 2
    double(x)
}
```
→ 
```haskell
EBlock 
    [TLLet "x" (EInt 10),
     TLFn "double" ["n"] (EBinary Mul (EVar "n") (EInt 2))]
    (Just (ECall (EVar "double") [EVar "x"]))
```

### Explicit Return

```flux
fn early(n) = {
    if n < 0 {
        RET 0
    } else {
        RET n
    }
}
```
→ 
```haskell
TLFn "early" ["n"] 
    (EBlock []
        (Just (EIf 
            (EBinary Lt (EVar "n") (EInt 0))
            (ERet (EInt 0))
            (ERet (EVar "n")))))
```

---

## Pipeline Desugaring

The pipeline operator `|>` is syntactic sugar. The parser transforms it before evaluation:

### Basic Pipeline
**Source:**
```flux
10 |> add5
```
**Parsed:** `EBinary Pipe (EInt 10) (EVar "add5")`  
**Desugared:** `ECall (EVar "add5") [EInt 10]`

### Chained Pipeline
**Source:**
```flux
10 |> add5 |> double
```
**Parsed:** `EBinary Pipe (EBinary Pipe (EInt 10) (EVar "add5")) (EVar "double")`  
**Desugared:** `ECall (EVar "double") [ECall (EVar "add5") [EInt 10]]`

### Tuple Unpacking in Pipeline
**Source:**
```flux
(5, 10) |> add
```
**Parsed:** `EBinary Pipe (ETuple [EInt 5, EInt 10]) (EVar "add")`  
**Desugared:** `ECall (EVar "add") [EInt 5, EInt 10]`

The tuple elements are unpacked as separate arguments!

### Pipeline with Partial Application
**Source:**
```flux
10 |> add(5)
```
**Parsed:** `EBinary Pipe (EInt 10) (ECall (EVar "add") [EInt 5])`  
**Desugared:** `ECall (EVar "add") [EInt 10, EInt 5]`

The piped value is **prepended** to the existing arguments.

---

## Complete Examples

### Example 1: Simple Arithmetic Function

**Source:**
```flux
fn add(a, b) = a + b
(5, 10) |> add |> print
```

**Parsed AST:**
```haskell
[TLFn "add" ["a","b"] (EBinary Add (EVar "a") (EVar "b")),
 TLExpr (EBinary Pipe (EBinary Pipe (ETuple [EInt 5, EInt 10]) (EVar "add")) (EVar "print"))]
```

**Desugared AST:**
```haskell
[TLFn "add" ["a","b"] (EBinary Add (EVar "a") (EVar "b")),
 TLExpr (ECall (EVar "print") [ECall (EVar "add") [EInt 5, EInt 10]])]
```

**Expected Output:** `15`

### Example 2: Factorial

**Source:**
```flux
fn fact(n) = if n <= 1 { 1 } else { n * fact(n - 1) }
fact(5) |> print
```

**Parsed AST:**
```haskell
[TLFn "fact" ["n"] 
    (EIf (EBinary Lte (EVar "n") (EInt 1))
         (EBlock [] (Just (EInt 1)))
         (EBlock [] (Just (EBinary Mul (EVar "n") 
            (ECall (EVar "fact") [EBinary Sub (EVar "n") (EInt 1)]))))),
 TLExpr (EBinary Pipe (ECall (EVar "fact") [EInt 5]) (EVar "print"))]
```

**Desugared AST:**
```haskell
[TLFn "fact" ["n"] 
    (EIf (EBinary Lte (EVar "n") (EInt 1))
         (EBlock [] (Just (EInt 1)))
         (EBlock [] (Just (EBinary Mul (EVar "n") 
            (ECall (EVar "fact") [EBinary Sub (EVar "n") (EInt 1)]))))),
 TLExpr (ECall (EVar "print") [ECall (EVar "fact") [EInt 5]])]
```

**Expected Output:** `120`

### Example 3: Map with Lambda

**Source:**
```flux
fn square(x) = x * x
[1, 2, 3, 4] |> map(square) |> print
```

**Parsed AST:**
```haskell
[TLFn "square" ["x"] (EBinary Mul (EVar "x") (EVar "x")),
 TLExpr (EBinary Pipe 
    (EBinary Pipe 
        (EList [EInt 1, EInt 2, EInt 3, EInt 4])
        (ECall (EVar "map") [EVar "square"]))
    (EVar "print"))]
```

**Desugared AST:**
```haskell
[TLFn "square" ["x"] (EBinary Mul (EVar "x") (EVar "x")),
 TLExpr (ECall (EVar "print") 
    [ECall (EVar "map") 
        [EList [EInt 1, EInt 2, EInt 3, EInt 4], 
         EVar "square"]])]
```

**Expected Output:** `[1,4,9,16]`

### Example 4: Local Bindings in Block

**Source:**
```flux
fn compute(x) = {
    let doubled = x * 2
    let squared = doubled * doubled
    squared + 1
}
compute(3) |> print
```

**Parsed AST:**
```haskell
[TLFn "compute" ["x"] 
    (EBlock 
        [TLLet "doubled" (EBinary Mul (EVar "x") (EInt 2)),
         TLLet "squared" (EBinary Mul (EVar "doubled") (EVar "doubled"))]
        (Just (EBinary Add (EVar "squared") (EInt 1)))),
 TLExpr (EBinary Pipe (ECall (EVar "compute") [EInt 3]) (EVar "print"))]
```

**Desugared AST:**
```haskell
[TLFn "compute" ["x"] 
    (EBlock 
        [TLLet "doubled" (EBinary Mul (EVar "x") (EInt 2)),
         TLLet "squared" (EBinary Mul (EVar "doubled") (EVar "doubled"))]
        (Just (EBinary Add (EVar "squared") (EInt 1)))),
 TLExpr (ECall (EVar "print") [ECall (EVar "compute") [EInt 3]])]
```

**Expected Output:** `37`

---

## Testing with --AST Flag

To see the actual parsed and desugared AST for any Flux program, use:

```bash
./flux --AST your_program.flux
```

This will display:
1. **PARSED AST** - The raw AST structure from the parser
2. **DESUGARED AST** - After pipeline transformations
3. **EXECUTION** - The normal program output

**Example:**
```bash
./flux --AST FLUX/examples/syntax_demo.flux
```

---

## Notes for Bytecode Compiler Implementation

### Key Considerations

1. **Closures**: Functions capture their environment. When compiling `TLFn` or `ELam`, track which variables are:
   - **Local parameters**: Direct stack access
   - **Free variables**: Need to be captured in closure environment
   
2. **Recursive Functions**: `TLFn` definitions can be self-referential. The closure must include a reference to itself in its environment.

3. **Pipeline Desugaring**: The `desugarPipes` function in `Parser.hs` shows the exact transformation rules. Your compiler can either:
   - Apply these transformations before bytecode generation
   - Or generate bytecode that implements pipeline semantics directly

4. **Type Tags**: Runtime values need type tags:
   - `VInt` → 0
   - `VBool` → 1
   - `VString` → 2
   - `VList` → 3
   - `VTuple` → 4
   - `VClosure` → 5
   - `VPrim` → 6 (builtin functions)

5. **Builtin Functions**: The interpreter defines these in `initialEnv`:
   - `print` - Takes 1 argument, prints and returns it
   - `map` - Takes 2 arguments (list, function), returns new list

   For bytecode, implement these as native functions (host calls to Java methods).

6. **Error Handling**: Operations can fail (type errors, arity mismatches, division by zero). Your VM needs an error mechanism (exceptions or error codes).

---

## Reference Implementation

- **Parser**: `FLUX/src/Parser.hs` - Shows how source text becomes AST
- **AST Types**: `FLUX/src/AST.hs` - Canonical AST structure definitions  
- **Interpreter**: `FLUX/src/Interpreter.hs` - Reference semantics for each AST node
- **Examples**: `FLUX/examples/*.flux` - Working programs to test against

To generate test cases for your compiler:
```bash
for f in FLUX/examples/*.flux; do
    echo "=== $f ==="
    ./flux --AST "$f"
done
```

This gives you both the expected AST structure and the expected output for validation.
