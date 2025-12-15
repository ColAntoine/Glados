# Flux Compiler & VM Architecture

This document describes a recommended implementation architecture for Flux, suitable for the GLaDOS project.

## Pipeline Overview
Source (text) -> Lexer/Parser -> AST -> Semantic checks & Desugaring -> Compiler -> Bytecode -> VM -> Output

## Components

### 1) Parser (Megaparsec)
- Parse functions, let-bindings, expressions, blocks, and pipeline operator.
- Produce precise parse errors with location.

### 2) AST
- Define algebraic data types for expressions:
  - Int, Float, Bool, String
  - Var, Let, If, Call, Lambda (if introduced), FunctionDef
  - Pipeline nodes can be represented as nested calls or a special node.

### 3) Semantic analysis
- Validate arity of top-level calls when possible.
- Ensure recursion works: allow binding name before evaluating body.
- Optional: type hints & basic static checks.

### 4) Desugaring
- Convert pipeline `a |> f |> g` → `g(f(a))` at AST stage.
- Convert function shorthand into canonical AST.

### 5) Compiler -> Bytecode
- Target a stack-based VM.
- Instruction set suggestions:
  - CONST_INT n, CONST_FLOAT f, CONST_BOOL b, CONST_STR s
  - LOAD_GLOBAL idx, STORE_GLOBAL idx
  - LOAD_LOCAL idx, STORE_LOCAL idx
  - CALL n, RET
  - JUMP addr, JUMP_IF_FALSE addr
  - PRIM_ADD, PRIM_SUB, PRIM_MUL, PRIM_DIV, PRIM_MOD
  - PRIM_EQ, PRIM_LT
  - HALT

- Each function becomes a flat code object (list of instructions).

### 6) VM
- Maintain a value stack and call frames with local slots.
- Builtins: print, arithmetic primitives, eq?, etc.
- Error handling: runtime errors print to stderr and exit 84.

## Files in `src/` (suggested Haskell modules)
- Parser.hs
- AST.hs
- Semantic.hs
- Compiler.hs
- Bytecode.hs
- VM.hs
- Builtins.hs
- Main.hs
