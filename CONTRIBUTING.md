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

## Release Process

GLaDOS uses an automated release workflow based on semantic versioning. Releases are triggered automatically when code is pushed to the `develop` or `main` branches.

### How It Works

- **Develop Branch** → **Minor Version Bump** (e.g., `0.1.0` → `0.2.0`)
  - Minor releases contain new features and improvements.
  - Triggered when a pull request is merged to `develop`.

- **Main Branch** → **Major Version Bump** (e.g., `0.2.0` → `1.0.0`)
  - Major releases indicate significant changes or breaking changes.
  - Triggered when code is merged/pushed to `main`.

### Automated Release Steps

When you push to `develop` or `main`, the GitHub Actions workflow (`.github/workflows/release.yml`) automatically:

1. Reads the current version from `package.yaml`
2. Bumps the version (minor for develop, major for main)
3. Updates `package.yaml` with the new version
4. Commits the version change
5. Creates a git tag (e.g., `v0.2.0`, `v1.0.0`)
6. Creates a GitHub Release with release notes

### Example Workflow

```bash
# Feature branch work
git checkout -b feature/new-parser
# ... make changes, commit, push ...
git push origin feature/new-parser

# Create a pull request to develop
# After review, merge to develop

# GitHub Actions automatically:
# - Bumps version from 0.1.5.0 → 0.2.0.0
# - Creates tag v0.2.0.0
# - Creates GitHub Release "v0.2.0.0 (Minor Release)"

# Later, when merging develop to main:
# - Bumps version from 0.2.0.0 → 1.0.0.0
# - Creates tag v1.0.0.0
# - Creates GitHub Release "v1.0.0.0 (Major Release)"

### Important Notes

- The version is stored in `package.yaml` under the `version:` field
- Only push to `develop` or `main` through pull requests (recommended)
- The release workflow runs after CI passes
- GitHub Actions automatically commits version changes back to the branch
- All releases are tagged and available in the [Releases](https://github.com/ColAntoine/Glados/releases) page

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
