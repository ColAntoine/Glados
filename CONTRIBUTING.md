# Contributing to GLaDOS

Thank you for your interest in contributing to the GLaDOS project! This document provides guidelines for setting up your development environment and contributing to the project.

## Prerequisites

Before you start contributing, make sure you have the following installed:

- **Haskell**: GHC 9.8.4 or compatible version
- **Stack**: Build tool for Haskell projects
- **Git**: Version control
- **Make**: Build automation tool
- **Pre-commit**: Git hook framework for managing and maintaining multi-language pre-commit hooks

## Setting Up Your Development Environment

### 1. Clone the Repository

```bash
git clone https://github.com/ColAntoine/Glados.git
cd Glados
```

### 2. Install Pre-commit Hooks

The project uses **pre-commit** to ensure code quality and run tests before committing. Install it:

```bash
# Using Homebrew (macOS)
brew install pre-commit

# Or using your package manager (Linux)
# For Ubuntu/Debian:
sudo apt-get install pre-commit

# Or Using pip
pip install pre-commit
```

### 3. Install Pre-commit Hooks for the Repository

Once pre-commit is installed, initialize the hooks for this repository:

```bash
pre-commit install
```

This will set up the pre-commit hooks defined in `.pre-commit-config.yaml`.

### 4. Build the Project

```bash
stack build
```

### 5. Run Tests

```bash
stack test
```

## Pre-commit Hooks

The project uses pre-commit hooks to maintain code quality. The following hooks run automatically before each commit:

1. **Run Haskell Tests**: Executes all test suites to ensure code functionality
2. **Clean Up Project**: Removes build artifacts and coverage files to keep the repository clean

If any hook fails, your commit will be blocked. Fix the issues and try committing again.

### Manual Pre-commit Execution

You can manually run pre-commit hooks at any time:

```bash
# Run all hooks
pre-commit run --all-files

# Run a specific hook
pre-commit run run-lisp-tests --all-files
```

## Workflow

1. **Create a branch** for your feature or fix:
   ```bash
   git checkout -b feature/my-feature
   ```

2. **Make your changes** and add tests if applicable

3. **Run tests locally** to ensure everything works:
   ```bash
   stack test
   ```

4. **Commit your changes**:
   ```bash
   git add .
   git commit -m "descriptive commit message"
   ```

   The pre-commit hooks will run automatically. If they fail, fix the issues and commit again.

5. **Push your branch** and create a pull request:
   ```bash
   git push origin feature/my-feature
   ```

## Writing Tests

For detailed information on how to write and structure tests for the project, see [test/TESTING.md](test/TESTING.md).

## Code Style

- Follow Haskell naming conventions
- Use meaningful variable and function names
- Add comments for complex logic
- Keep functions small and focused

## Troubleshooting

### Pre-commit Installation Issues

If you encounter issues installing pre-commit, try:

```bash
pip install --upgrade pre-commit
```

### Pre-commit Hooks Not Running

Make sure the hooks are installed:

```bash
pre-commit install
```

Check the hook configuration:

```bash
cat .git/hooks/pre-commit
```

### Build Failures

If you encounter build failures:

```bash
# Clean previous builds
stack clean

# Remove coverage files
rm -f glados-test.tix

# Rebuild
stack build
```

## Questions or Issues?

If you have questions or run into issues, please open an issue on the repository or contact the maintainers.

Thank you for contributing to GLaDOS!
