# Testing Guide for GLaDOS

This guide explains how to write and run tests for the GLaDOS project.

## Overview

GLaDOS uses **Tasty** as the test framework combined with **Hspec** for writing expressive test specifications. Tests are organized by feature/functionality in subdirectories under `test/LispCases/`.

## Test Structure

Each test category has its own subdirectory with:

- **`TestName.hs`**: Haskell test module containing test specifications
- **`*.scm`**: Scheme/Lisp test files to be executed and validated

Example structure:
```
test/
├── Test.hs                          # Main test runner
└── LispCases/
    ├── Call/
    │   ├── Call.hs                  # Test module
    │   └── call.scm                 # Test data
    ├── If/
    │   ├── If.hs                    # Test module
    │   ├── if1.scm
    │   ├── if2.scm
    │   └── if3.scm
    └── Lambda/
        ├── Lambda.hs                # Test module
        ├── lambda1.scm
        ├── lambda2.scm
        └── lambda3.scm
```

## Writing Tests

### Step 1: Create Test Data Files

Create `.scm` (Scheme) files in your test subdirectory:

```scheme
;; test/LispCases/MyFeature/myfeature.scm
(+ 2 3)
```

### Step 2: Create Test Module

Create a Haskell test file (e.g., `MyFeature.hs`) in the same directory:

```haskell
module LispCases.MyFeature.MyFeature (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import System.Process (readProcess)
import Control.Exception (catch, SomeException)

-- Required export
tests :: IO TestTree
tests = testSpec "MyFeature" spec

-- Write your test specifications here
spec :: Spec
spec = do
    it "should evaluate (+ 2 3) to 5" $ do
        result <- catch (readProcess "glados" ["-lisp"] "(+ 2 3)") handleError
        result `shouldBe` "5\n"
    
    it "should handle myfeature.scm" $ do
        schemeCode <- readFile "test/LispCases/myfeature/myfeature.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""

-- Error handler
handleError :: SomeException -> IO String
handleError e = return $ "Error: " ++ show e
```

### Step 3: Register Test in Main Test Runner

Add your test to `test/Test.hs`:

```haskell
import qualified LispCases.MyFeature.MyFeature as MyFeatureTest

main :: IO ()
main = do
  -- ... other tests ...
  myFeatureTests <- MyFeatureTest.tests
  defaultMain $ testGroup "Glados Tests"
    [ -- ... other tests ...
    , myFeatureTests
    ]
```

## Running Tests

### Run All Tests

```bash
stack test
```

### Run Tests with Coverage

```bash
stack test --coverage
```

This generates coverage reports showing which parts of your code are tested.

### Run Tests in Watch Mode

```bash
stack test --file-watch
```

This automatically re-runs tests when you save files.

## Test Patterns

### Pattern 1: Simple Expression Evaluation

```haskell
it "should evaluate a simple expression" $ do
    result <- catch (readProcess "glados" ["-lisp"] "(+ 1 2)") handleError
    result `shouldBe` "3\n"
```

### Pattern 2: Load from File

```haskell
it "should handle test file" $ do
    code <- readFile "test/LispCases/factorial/factorial.scm"
    result <- catch (readProcess "glados" ["-lisp"] code) handleError
    result `shouldNotBe` ""
```

### Pattern 3: Multiple Test Cases

```haskell
spec :: Spec
spec = do
    it "case 1" $ do
        result <- catch (readProcess "glados" ["-lisp"] "(* 2 3)") handleError
        result `shouldBe` "6\n"
    
    it "case 2" $ do
        result <- catch (readProcess "glados" ["-lisp"] "(* 4 5)") handleError
        result `shouldBe` "20\n"
```

### Pattern 4: Error Handling

```haskell
it "should handle errors gracefully" $ do
    result <- catch (readProcess "glados" ["-lisp"] "(undefined-function)") handleError
    result `shouldContain` "Error"
```

## Useful Hspec Assertions

- `shouldBe`: Check equality
  ```haskell
  result `shouldBe` "expected value"
  ```

- `shouldNotBe`: Check inequality
  ```haskell
  result `shouldNotBe` ""
  ```

- `shouldContain`: Check substring
  ```haskell
  result `shouldContain` "substring"
  ```

- `shouldNotContain`: Check substring absence
  ```haskell
  result `shouldNotContain` "substring"
  ```

- `shouldThrow`: Check exceptions
  ```haskell
  (readProcess "glados" ["-lisp"] "(error)") `shouldThrow` anyException
  ```

## Best Practices

1. **Organize by Feature**: Keep tests in subdirectories organized by feature or functionality

2. **Descriptive Names**: Use clear, descriptive test names that explain what is being tested
   ```haskell
   it "should evaluate (div 10 2) to 5" $ do
   ```

3. **One Assertion Per Test**: Each test should verify one specific behavior

4. **Use Test Data Files**: For complex test cases, use `.scm` files instead of inline code

5. **Handle Exceptions**: Always use `catch` when calling external processes

6. **Meaningful Error Messages**: Make error messages helpful for debugging

## Adding a New Test Directory

1. Create a new directory under `test/LispCases/`:
   ```bash
   mkdir test/LispCases/NewFeature
   ```

2. Create test data files (`.scm`):
   ```bash
   touch test/LispCases/NewFeature/newfeature.scm
   ```

3. Create the test module `NewFeature.hs`:
   ```bash
   touch test/LispCases/NewFeature/NewFeature.hs
   ```

4. Register in `test/Test.hs`:
   ```haskell
   import qualified LispCases.NewFeature.NewFeature as NewFeatureTest
   
   main :: IO ()
   main = do
     newFeatureTests <- NewFeatureTest.tests
     defaultMain $ testGroup "Glados Tests"
       [ newFeatureTests
       ]
   ```

## Coverage Reports

After running tests with coverage:

```bash
stack test --coverage
```

View the coverage report:

```bash
# Generate HTML coverage report
hpc markup .stack-work/dist/x86_64-linux/ghc-9.8.4/build/glados/glados-test/hpc/vanilla/html/glados-test.hs.html
```

## Troubleshooting

### Test File Not Found

Make sure the path is relative to your project root:
```haskell
readFile "test/LispCases/foo/foo.scm"  -- Correct
readFile "foo.scm"                      -- Wrong
```

### Module Not Found

Ensure your module path matches the file location:
- File: `test/LispCases/MyFeature/MyFeature.hs`
- Module: `module LispCases.MyFeature.MyFeature`

### Tests Not Running

Make sure your test module:
1. Exports `tests :: IO TestTree`
2. Is imported in `test/Test.hs`
3. Is run in the `main` function

### Build Failures

Clean and rebuild:
```bash
stack clean
rm -f glados-test.tix
stack test
```

## Questions?

If you have questions about writing tests, please refer to:
- [Tasty Documentation](https://hackage.haskell.org/package/tasty)
- [Hspec Documentation](http://hspec.github.io/)
- Existing test examples in `test/LispCases/`
