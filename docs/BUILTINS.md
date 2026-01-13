# Flux Builtin Functions

This document describes the builtin functions available in Flux and their support in different execution modes.

## Execution Modes

Flux supports two execution modes:
- **Interpreter Mode** (`-i`): Full support for all builtin functions
- **Compiler Mode** (`-c`): Currently limited support (see below)

## Builtin Functions

### ✅ Fully Supported (Interpreter & Compiler)

#### Core Functions
- **`print(value)`** - Print a value to stdout
  - Strings are printed as-is
  - Other types get formatted and a newline added

### ✅ Interpreter Only (Currently)

The following functions work in interpreter mode but are not yet implemented in the compiler:

#### String/List Operations
- **`len(str|list)`** - Get length of string or list
- **`head(list|str)`** - Get first element
- **`tail(list|str)`** - Get all elements except first
- **`at(list|str, index)`** - Get element at index
- **`concat(str, str)` or `concat(list, list)`** - Concatenate two strings or two lists

#### File I/O
- **`readFile(path)`** - Read file contents as string
- **`writeFile(path, content)`** - Write string to file
- **`appendFile(path, content)`** - Append string to file

#### String Operations
- **`charAt(str, index)`** - Get character at index
- **`substring(str, start, end)`** - Extract substring
- **`toUpper(str)`** - Convert to uppercase
- **`toLower(str)`** - Convert to lowercase
- **`split(str, delimiter)`** - Split string into list
- **`join(list, separator)`** - Join list of strings

#### Math Operations
- **`abs(n)`** - Absolute value
- **`min(a, b)`** - Minimum of two numbers
- **`max(a, b)`** - Maximum of two numbers
- **`pow(base, exp)`** - Exponentiation (integer only)

#### Type Checking
- **`isInt(value)`** - Check if value is integer
- **`isBool(value)`** - Check if value is boolean
- **`isString(value)`** - Check if value is string
- **`isList(value)`** - Check if value is list

#### List Operations
- **`reverse(list|str)`** - Reverse a list or string
- **`filter(fn, list)`** - Filter list with predicate function
- **`fold(fn, initial, list)`** - Reduce list with binary function
- **`range(end)` or `range(start, end)`** - Generate list of integers
- **`map(fn, list)`** - Map function over list (also works in compiler)

## Usage Examples

### Interpreter Mode
```bash
# All builtins work
./glados -i examples/builtin_demo.flux
./glados -i examples/builtin_file_io.flux
./glados -i examples/builtin_math.flux
```

### Compiler Mode
```bash
# Use basic features and print
./glados -c examples/compiler_demo.flux
./examples/compiler_demo
```

## Example Files

- `builtin_demo.flux` - Comprehensive demo of all builtins (interpreter only)
- `builtin_file_io.flux` - File I/O operations with map.txt
- `builtin_len.flux` - Length function examples
- `builtin_head_tail.flux` - List head/tail operations
- `builtin_at.flux` - Index access examples
- `builtin_concat.flux` - String and list concatenation
- `builtin_string_ops.flux` - String manipulation functions
- `builtin_split_join.flux` - String splitting and joining
- `builtin_math.flux` - Math operations
- `builtin_type_check.flux` - Type checking functions
- `builtin_reverse.flux` - Reverse lists and strings
- `builtin_filter.flux` - Filter with predicates
- `builtin_fold.flux` - Fold/reduce operations
- `builtin_range.flux` - Range generation
- `compiler_demo.flux` - Features that work with compiler

## Notes

1. **Concat limitation**: `concat` only works with two strings or two lists. To concat a string with a number, convert the number first (not yet implemented) or use multiple print statements.

2. **Compiler support**: The compiler currently has limited builtin support. A runtime library (`runtime/flux_runtime.c`) has been started but is not yet integrated into the compilation process.

3. **Future work**: Full compiler support for all builtins requires:
   - Declaring external C functions in LLVM IR
   - Implementing builtin call code generation
   - Linking the runtime library during compilation
   - Or implementing builtins directly in LLVM IR

## Testing

Run the test suite to verify builtin functions:
```bash
# Interpreter tests
./glados -i examples/builtin_len.flux
./glados -i examples/builtin_math.flux
./glados -i examples/builtin_filter.flux

# Compiler tests
./glados -c examples/compiler_demo.flux
./examples/compiler_demo
```
