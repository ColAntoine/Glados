# Flux Builtin Functions

This document describes the builtin functions available in Flux.

## Execution Modes

Flux supports two execution modes:
- **Interpreter Mode** (`-i`): Full support for all builtin functions
- **Compiler Mode** (`-c`): All builtins now compile to native LLVM IR

## Builtin Functions

### ✅ Fully Supported (Interpreter & Compiler)

#### Core Functions
- **`print(value)`** - Print a value to stdout
  - Strings are printed as-is
  - Integers, booleans, and lists are formatted

#### String/List Operations
- **`len(str|list)`** - Get length of string or list
- **`head(list|str)`** - Get first element
- **`tail(list|str)`** - Get all elements except first
- **`at(list|str, index)`** - Get element at index
- **`concat(str, str)` or `concat(list, list)`** - Concatenate two strings or two lists
- **`reverse(list|str)`** - Reverse a list or string

#### File I/O
- **`readFile(path)`** - Read file contents as string
- **`writeFile(path, content)`** - Write string to file (overwrites)
- **`appendFile(path, content)`** - Append string to file

#### String Operations
- **`substring(str, start, end)`** - Extract substring from start (inclusive) to end (exclusive)
- **`toUpper(str)`** - Convert string to uppercase
- **`toLower(str)`** - Convert string to lowercase

#### Math Operations
- **`abs(n)`** - Absolute value
- **`min(a, b)`** - Minimum of two numbers
- **`max(a, b)`** - Maximum of two numbers
- **`pow(base, exp)`** - Exponentiation (integer only)

#### Type Checking
- **`isInt(value)`** - Check if value is integer (returns boolean)
- **`isBool(value)`** - Check if value is boolean (returns boolean)
- **`isString(value)`** - Check if value is string (returns boolean)
- **`isList(value)`** - Check if value is list (returns boolean)

## Usage Examples

### Interpreter Mode
```bash
./glados -i examples/builtin_demo.flux
./glados -i examples/builtin_file_io.flux
./glados -i examples/builtin_math.flux
```

### Compiler Mode
```bash
# All builtins now work in compiler mode
./glados -c examples/builtin_file_io.flux
./builtin_file_io

./glados -c examples/builtin_math.flux
./builtin_math

./glados -c examples/bsq.flux
./bsq
```

## Example Files

- `builtin_demo.flux` - Comprehensive demo of various builtins
- `builtin_file_io.flux` - File I/O operations with map.txt
- `builtin_len.flux` - Length function examples
- `builtin_head_tail.flux` - List head/tail operations
- `builtin_at.flux` - Index access examples
- `builtin_concat.flux` - String and list concatenation
- `builtin_string_ops.flux` - String manipulation functions (toUpper, toLower, substring)
- `builtin_math.flux` - Math operations (abs, min, max, pow)
- `builtin_type_check.flux` - Type checking functions
- `builtin_reverse.flux` - Reverse lists and strings
- `bsq.flux` - Biggest Square algorithm using file I/O

## Implementation Notes

1. **String Comparison**: String equality (`==`) uses `strcmp` to compare actual string content, not just pointers.

2. **Inline LLVM**: All builtins are implemented as inline LLVM IR generation - no external C runtime required.

3. **Type Safety**: Builtins handle both their expected types (e.g., `head` works on both strings and lists).

4. **File I/O**: Uses standard C library functions (fopen, fread, fwrite, fclose) for file operations.

## Testing

Run the test suite to verify builtin functions:
```bash
# Test all builtins
stack test

# Individual examples
./glados -c examples/builtin_len.flux && ./builtin_len
./glados -c examples/builtin_math.flux && ./builtin_math
./glados -c examples/bsq.flux && ./bsq
```
