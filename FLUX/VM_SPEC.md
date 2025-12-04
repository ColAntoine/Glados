# VM Specification for Flux

## Architecture
- Stack-based VM
- Value stack containing runtime `Value` values
- Call frames containing local variables and return address

## Instructions (summary)
- CONST_INT n     -- push integer n
- CONST_FLOAT f   -- push float f
- CONST_BOOL b    -- push boolean
- CONST_STR s     -- push string s
- LOAD_GLOBAL i   -- push value of global index i
- STORE_GLOBAL i  -- pop and store into global i
- LOAD_LOCAL i    -- push local variable i
- STORE_LOCAL i   -- pop and store into local i
- CALL n          -- call function with n arguments (callee is on stack)
- RET             -- return from function, value on top is return value
- JUMP addr
- JUMP_IF_FALSE addr
- PRIM_ADD, PRIM_SUB, PRIM_MUL, PRIM_DIV, PRIM_MOD
- PRIM_EQ, PRIM_LT
- HALT

## Errors
- Division by zero -> runtime error (stderr), exit code 84
- Type errors -> runtime error, exit code 84
- Unbound variable -> runtime error, exit code 84
