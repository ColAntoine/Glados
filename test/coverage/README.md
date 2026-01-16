# Coverage Testing Suite

This directory contains comprehensive coverage tests for the Flux language implementation. These tests are separate from the CI test suite and focus on achieving maximum code coverage.

## Purpose

- Achieve comprehensive code coverage of the Flux interpreter and compiler
- Test edge cases and error conditions not covered by CI tests
- Verify behavior of all language features in various combinations
- Document expected behavior through extensive test cases

## Structure

Tests are organized by the source module they cover:

- **`Parser/`** - Tests for `src/Parser.hs`
  - Lexer edge cases
  - Parser error recovery
  - Complex expression parsing
  - Syntactic sugar handling
  
- **`Interpreter/`** - Tests for `src/Interpreter.hs`
  - Runtime evaluation edge cases
  - Built-in function coverage
  - Error handling and exceptions
  - Environment and scope testing
  
- **`Compiler/`** - Tests for `src/Compiler.hs` and compiler modules
  - Code generation coverage
  - Optimization paths
  - Type boxing/unboxing
  - Expression compilation
  
- **`AST/`** - Tests for `src/AST.hs`
  - AST construction and manipulation
  - Node type coverage
  - AST transformations

## Running Coverage Tests

```bash
# Run coverage tests and generate HPC report
stack clean
stack test --coverage glados:coverage-tests

# View coverage report
stack hpc report glados-coverage
```

## Coverage Goals

- **Primary Goal**: 90%+ line coverage across all modules
- **Secondary Goal**: 80%+ branch coverage
- **Tertiary Goal**: 100% function coverage

## Notes

- These tests are NOT run in CI
- Tests can be slower/more comprehensive than CI tests
- Focus on coverage completeness rather than speed
- Each test file should document which code paths it covers
