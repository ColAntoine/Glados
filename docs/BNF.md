# Flux Language BNF Grammar Specification

This document provides the formal Backus-Naur Form (BNF) grammar specification for the Flux programming language.

## Notation Conventions

- `<symbol>` : Non-terminal symbol
- `"text"` : Terminal symbol (literal text)
- `|` : Alternative (OR)
- `[]` : Optional (zero or one occurrence)
- `{}` : Repetition (zero or more occurrences)
- `()` : Grouping

## Grammar Rules

### Program Structure

```bnf
<program> ::= {<top-level>}

<top-level> ::= <import-stmt>
              | <function-def>
              | <procedure-def>
              | <let-binding>
              | <expression>

<import-stmt> ::= "import" <identifier>
```

### Function and Procedure Definitions

```bnf
<function-def> ::= "fn" <identifier> "(" <param-list> ")" "=" <expression>

<procedure-def> ::= "fn" <identifier> "(" <param-list> ")" <block>

<param-list> ::= [<identifier> {"," <identifier>}]
```

### Let Bindings

```bnf
<let-binding> ::= "let" <identifier> "=" <expression>
```

### Expressions

```bnf
<expression> ::= <pipe-expr>

<pipe-expr> ::= <logical-or-expr> {"|>" <logical-or-expr>}

<logical-or-expr> ::= <logical-and-expr> {"||" <logical-and-expr>}

<logical-and-expr> ::= <comparison-expr> {"&&" <comparison-expr>}

<comparison-expr> ::= <additive-expr> [<comparison-op> <additive-expr>]

<comparison-op> ::= "==" | "!=" | "<" | "<=" | ">" | ">="

<additive-expr> ::= <multiplicative-expr> {<additive-op> <multiplicative-expr>}

<additive-op> ::= "+" | "-"

<multiplicative-expr> ::= <unary-expr> {<multiplicative-op> <unary-expr>}

<multiplicative-op> ::= "*" | "/" | "%"

<unary-expr> ::= <unary-op> <unary-expr>
               | <call-expr>

<unary-op> ::= "-" | "!"

<call-expr> ::= <primary-expr> {<call-suffix>}

<call-suffix> ::= "(" <argument-list> ")"

<argument-list> ::= [<expression> {"," <expression>}]

<primary-expr> ::= <integer-literal>
                 | <boolean-literal>
                 | <string-literal>
                 | <identifier>
                 | <list-literal>
                 | <tuple-literal>
                 | <lambda-expr>
                 | <if-expr>
                 | <block>
                 | <return-expr>
                 | "(" <expression> ")"
```

### Control Flow

```bnf
<if-expr> ::= "if" <expression> <block> ["else" (<block> | <if-expr>)]

<return-expr> ::= "RET" <expression>

Note: Return statements use the RET keyword, not 'return'.
```

### Lambda Expressions

```bnf
<lambda-expr> ::= "(" <param-list> ")" "=>" <expression>
```

Note: Lambda expressions can only contain single expressions, not blocks. For complex logic, use function definitions.

### Blocks

```bnf
<block> ::= "{" {<top-level>} [<expression>] "}"

Note: Blocks are only valid inside function bodies, not as standalone expressions in let bindings or other contexts. Nested anonymous blocks are not supported.
```



### Literals

```bnf
<integer-literal> ::= ["-"] <digit> {<digit>}

<boolean-literal> ::= "true" | "false"

<string-literal> ::= '"' {<string-char>} '"'

<string-char> ::= <any-char-except-quote-or-backslash>
                | "\\" <escape-sequence>

<escape-sequence> ::= "n" | "t" | "r" | '"' | "\\"

<list-literal> ::= "[" [<expression> {"," <expression>}] "]"

<tuple-literal> ::= "(" <expression> "," <expression> {"," <expression>} ")"
```

### Identifiers

```bnf
<identifier> ::= <letter> {<letter> | <digit> | "_"}

<letter> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
```

### Comments

```bnf
<single-line-comment> ::= "//" {<any-char-except-newline>} <newline>

<multi-line-comment> ::= "/*" {<any-char>} "*/"
```

## Complete Example with Grammar Annotations

```flux
// Function definition: <function-def>
fn factorial(n) = {
    // <if-expr>
    if n <= 1 {
        1  // <integer-literal>
    } else {
        // <binary-expr> with <call-expr>
        n * factorial(n - 1)
    }
}

// Top-level expression: <expression>
factorial(10) |> print
```

**Grammar trace:**
1. `factorial(10)` → `<call-expr>` → `<identifier>` + `"(" <argument-list> ")"`
2. `|>` → `<pipe-expr>` operator
3. `print` → `<identifier>` (builtin function reference)

## Operator Precedence (Highest to Lowest)

1. **Primary**: Literals, identifiers, parentheses, function calls
2. **Unary**: `-` (negation), `!` (logical NOT)
3. **Multiplicative**: `*`, `/`, `%`
4. **Additive**: `+`, `-`
5. **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
6. **Logical AND**: `&&`
7. **Logical OR**: `||`
8. **Pipeline**: `|>`

## Associativity Rules

- Binary operators (`+`, `-`, `*`, `/`, `%`, `&&`, `||`) are **left-associative**
- Pipeline operator (`|>`) is **left-associative**
- Unary operators (`-`, `!`) are **right-associative**
- Function application is **left-associative**

## Syntax Notes

### Function vs Procedure

```bnf
// Function (returns a value) - requires '='
<function-def> ::= "fn" <identifier> "(" <params> ")" "=" <expression>

// Procedure (no return) - no '='
<procedure-def> ::= "fn" <identifier> "(" <params> ")" <block>
```

### Block Expressions

Blocks can only be used inside function bodies. A block can contain top-level forms (function definitions, let bindings) and optionally ends with an expression that becomes the block's value:

```flux
fn example() = {
    let x = 10
    fn helper(n) = n + x
    helper(5)  // block evaluates to 15
}
```

Note: Blocks cannot be used as standalone expressions in let bindings or nested anonymously.

### Pipeline Desugaring

The pipeline operator is syntactic sugar:

```bnf
<expr1> |> <expr2>  ≡  <expr2>(<expr1>)
```

Examples:
```flux
// These are equivalent:
5 |> factorial
factorial(5)

// Chain pipelines:
10 |> double |> print
print(double(10))

// With lambdas:
5 |> (x) => x * 2
(x) => x * 2 (5)
```

### Tuple Unpacking

When a tuple is passed as an argument to a function, it can be automatically unpacked:

```flux
fn add(a, b) = a + b
(5, 10) |> add  // unpacks to add(5, 10)
```

### Recursion (No Loops)

Flux is a functional language and uses **recursion** instead of loops. See examples in [SYNTAX.md](SYNTAX.md#4-recursion-iteration-in-functional-style).

## Extended BNF (EBNF) Alternative Notation

For those familiar with EBNF notation:

```ebnf
program = { top_level } ;

top_level = import_stmt 
          | function_def 
          | procedure_def 
          | let_binding 
          | expression ;

import_stmt = "import" identifier ;

function_def = "fn" identifier "(" [ param_list ] ")" "=" expression ;

procedure_def = "fn" identifier "(" [ param_list ] ")" block ;

param_list = identifier { "," identifier } ;

expression = pipe_expr ;

pipe_expr = logical_or_expr { "|>" logical_or_expr } ;

logical_or_expr = logical_and_expr { "||" logical_and_expr } ;

logical_and_expr = comparison_expr { "&&" comparison_expr } ;

comparison_expr = additive_expr [ comparison_op additive_expr ] ;

comparison_op = "==" | "!=" | "<" | "<=" | ">" | ">=" ;

additive_expr = multiplicative_expr { ( "+" | "-" ) multiplicative_expr } ;

multiplicative_expr = unary_expr { ( "*" | "/" | "%" ) unary_expr } ;

unary_expr = ( "-" | "!" ) unary_expr 
           | call_expr ;

call_expr = primary_expr { "(" [ argument_list ] ")" } ;

primary_expr = integer_literal
             | boolean_literal
             | string_literal
             | identifier
             | list_literal
             | tuple_literal
             | lambda_expr
             | if_expr
             | block
             | return_expr
             | "(" expression ")" ;

if_expr = "if" expression block [ "else" ( block | if_expr ) ] ;

block = "{" { top_level } [ expression ] "}" ;

lambda_expr = "(" param_list ")" "=>" expression ;

list_literal = "[" [ expression { "," expression } ] "]" ;

tuple_literal = "(" expression "," expression { "," expression } ")" ;

identifier = letter { letter | digit | "_" } ;

integer_literal = [ "-" ] digit { digit } ;

boolean_literal = "true" | "false" ;

string_literal = '"' { string_char } '"' ;
```

## Lexical Structure

### Keywords (Reserved Words)

```
fn      if      else    let     import
true    false   RET
```

Note: The `from` keyword is not used in the current import syntax.

### Operators and Delimiters

```
+  -  *  /  %           // Arithmetic
== != <  <= >  >=       // Comparison
&& ||  !                // Logical
|>                      // Pipeline
=                       // Assignment/Definition
=>                      // Lambda
( ) [ ] { }             // Delimiters
,  ;                    // Separators
```

### Whitespace

Whitespace (spaces, tabs, newlines) is generally insignificant except:
- To separate tokens
- Newlines can terminate statements in some contexts
- Indentation is **not significant** (unlike Python)

### Comments

- Single-line: `// comment text`
- Multi-line: `/* comment text */`

## Common Grammar Patterns

### Recursive Function Definition

```flux
fn factorial(n) = {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}
```

**Grammar derivation:**
```
<function-def>
  → "fn" <identifier> "(" <param-list> ")" "=" <expression>
  → "fn" "factorial" "(" "n" ")" "=" <block>
  → <block> contains <if-expr>
    → <if-expr> has condition (n <= 1), then-branch (1), else-branch (n * recursive call)
```

### List Processing with Recursion

```flux
fn sumList(list) = {
    if len(list) == 0 {
        0
    } else {
        head(list) + sumList(tail(list))
    }
}
```

### Higher-Order Functions

```flux
fn map(f, list) = {
    if len(list) == 0 {
        []
    } else {
        [f(head(list))] ++ map(f, tail(list))
    }
}

fn double(x) = x * 2
map(double, [1, 2, 3]) |> print

// With lambda:
map((x) => x * 2, [1, 2, 3]) |> print
```

## See Also

- [SYNTAX.md](SYNTAX.md) - Detailed syntax examples and use cases
- [AST.md](AST.md) - Abstract Syntax Tree structure
- [README.md](README.md) - Language overview and building instructions
