**FLUX — Build & AST**

- **Build:**: To build the Flux interpreter from the repository root run:

  ```bash
  make flux
  ```

  This runs `stack build` in the `FLUX/` subproject and copies the `flux` executable to the repo root. You can also build directly inside the `FLUX` folder:

  ```bash
  cd FLUX
  stack build
  ```

- **Run:**: Execute a Flux program file with:

  ```bash
  ./flux FLUX/examples/your_program.flux
  ```

  The interpreter prints results with the `print` builtin and returns an exit code `84` on runtime error (matching the Glados/subject conventions).

- **Debug AST:**: To see the parsed and desugared AST before execution, use the `--AST` flag:

  ```bash
  ./flux --AST FLUX/examples/your_program.flux
  ```

  This will display:
  - **PARSED AST**: The raw AST as parsed from source code
  - **DESUGARED AST**: The AST after pipeline desugaring transformations
  - **EXECUTION**: Then the normal program output

  This is extremely useful for understanding how Flux code maps to AST structures when designing a bytecode compiler. See `AST.md` for detailed AST documentation with examples.

**Quick Notes**
- Pipeline operator `|>` is desugared by the parser to a call form: `a |> b` becomes `b(a)` (or `b(a, ...)` if `b` is a call).
- Lists are first-class (`[1,2,3]`) and lambdas/closures capture their lexical environment.

**AST Overview (for bytecode design)**

The Flux AST is defined in Haskell (see `FLUX/src/AST.hs`). Below are the important node types and suggestions for mapping them to bytecode.

- **Program / Top-level**
  - `type Program = [TopLevel]`
  - `data TopLevel = TLFn String [String] Expr | TLLet String Expr | TLExpr Expr`
  - Top-level `TLFn` forms introduce named functions (closures). `TLLet` binds values at top-level.

- **Expressions (`Expr`)** — common nodes:
  - `EInt Int64` : integer literal
  - `EBool Bool` : boolean literal (`true` / `false`)
  - `EVar String` : variable reference
  - `EList [Expr]` : list literal
  - `ELam [String] Expr` : anonymous function (lambda)
  - `EIf Expr Expr Expr` : conditional
  - `ECall Expr [Expr]` : function call (function expression + arguments)
  - `EUnary String Expr` : unary operators (e.g. `-`, `!`)
  - `EBinary Op Expr Expr` : binary operators where `Op` is one of `Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Lte, Gt, Gte, And, Or, Pipe`
  - `EBlock [TopLevel] (Maybe Expr)` : lexical block with its own top-level forms and optional final expression

- **Operator note:** `Pipe` is desugared by parser via `desugarPipes` before evaluation, so on bytecode side you can treat pipes as calls.

**Runtime Value model**
- The interpreter uses these runtime values (map them to your bytecode value tags):
  - `VInt` → integer tag
  - `VBool` → boolean tag
  - `VList [Value]` → list/array tag
  - `VClosure [String] Expr Env` → closure: code pointer + list of parameter names + captured environment (closed-over values)
  - `VPrim ([Value] -> IO (Either String Value))` → builtin/native function (for Java backend, implement as host-call)

When compiling to bytecode, closures must capture the environment (either by storing pointers/indexes to captured variables, or by copying a small environment array). Each closure value on the heap should include an entry describing captured variables.

**Suggested bytecode design (simple stack-machine outline)**

- Value stack based instructions (examples):
  - `PUSH_INT n` — push integer
  - `PUSH_BOOL b` — push bool
  - `PUSH_LIST N` — pop N values and push a list
  - `LOAD_LOCAL i` / `STORE_LOCAL i` — locals access
  - `LOAD_FREE i` / `STORE_FREE i` — captured env access for closures
  - `MAKE_CLOSURE code_label arity free_count` — create closure capturing `free_count` values from stack
  - `CALL n` — call callable at stack top with n args
  - `RETURN` — return from function
  - `JMP_IF_FALSE label`, `JMP label` — control flow for `if`

- Closure layout: [code_label, arity, captured_values...] — the VM when calling a closure must set up a new frame with captured values accessible as `LOAD_FREE`.

**Example mapping**
- Flux source (factorial snippet):

  ```flux
  fn fact(n) = if n <= 1 then 1 else n * fact(n - 1)
  print(fact(10)) or 10 |> fact |> print
  ```

- High-level compilation steps:
  1. Compile `fact` body to bytecode with a symbolic label `L_fact`.
  2. `MAKE_CLOSURE L_fact 1 0` and `STORE_GLOBAL "fact"` (no free vars here).
  3. Compile call `fact(10)` as: `PUSH_INT 10`, `LOAD_GLOBAL "fact"`, `CALL 1`.
  4. `CALL` transfers control to `L_fact` with a new frame containing `n`.

**Notes for a Java backend**
- Represent values as a small class hierarchy (e.g., `VInt`, `VBool`, `VList`, `VClosure`, `VPrim`). Use a host-call interface for `VPrim` builtins (Java methods). Closures can be objects holding a reference to a code descriptor (e.g., an enum or label) and an array of captured values.

**Where to look in the code**
- Parser: `FLUX/src/Parser.hs` (parsing, tokenization, `desugarPipes`)
- AST types: `FLUX/src/AST.hs`
- Interpreter: `FLUX/src/Interpreter.hs` (runtime behavior and `initialEnv` builtins)

If your friend wants, I can also:
- produce a small JSON description of the AST for a file (AST → JSON) so they can consume and compile to bytecode on their side; or
- create a minimal bytecode instruction list and a small reference VM in Java to run the compiled bytecode.

If you want one of those, tell me which and I'll implement it next.
