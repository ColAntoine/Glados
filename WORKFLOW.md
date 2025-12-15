# GLaDOS Workflow Documentation

This document describes the complete development, testing, and release workflow for GLaDOS.

## Table of Contents

1. [Branch Strategy](#branch-strategy)
2. [Pull Request Workflow](#pull-request-workflow)
3. [Testing Pipeline](#testing-pipeline)
4. [Release System](#release-system)
5. [GitHub Actions Workflows](#github-actions-workflows)

---

## Branch Strategy

GLaDOS uses a **git flow** branching model with semantic versioning:

### Main Branches

- **`main`** — Production-ready code
  - Protected branch (requires 3 approvals to merge)
  - Direct pushes forbidden
  - Merging triggers a **MAJOR version bump** (e.g., `0.2.0` → `1.0.0`)
  - All code here is stable and released

- **`develop`** — Development branch
  - Integration branch for features
  - Merging triggers a **MINOR version bump** (e.g., `0.1.0` → `0.2.0`)
  - Code here is tested but may contain experimental features

### Feature Branches

```
feature/description       — New features
fix/description          — Bug fixes
refactor/description     — Code refactoring
chore/description        — Build, dependencies, tooling
test/description         — Test improvements
```

Example:
```bash
git checkout -b feature/new-parser-combinator
git checkout -b fix/parser-edge-case
git checkout -b test/add-integration-tests
```

---

## Pull Request Workflow

### Creating a Pull Request

1. **Create a feature branch** from `develop`:
   ```bash
   git checkout develop
   git pull origin develop
   git checkout -b feature/my-feature
   ```

2. **Make your changes** and commit with clear messages:
   ```bash
   git add .
   git commit -m "feat: add new parser combinator"
   ```

3. **Run tests locally** before pushing:
   ```bash
   stack test
   ```

4. **Push your branch**:
   ```bash
   git push origin feature/my-feature
   ```

5. **Create a Pull Request** on GitHub:
   - Target: `develop` (for features/fixes)
   - Add a descriptive title and description
   - Link any related issues

### PR Review & Merge

- **For `develop` branch:**
  - Requires CI checks to pass (tests must pass)
  - Code review recommended
  - Can be merged by any contributor with write access

- **For `main` branch:**
  - Requires **3 approvals** from different reviewers
  - Requires CI checks to pass
  - Requires `develop` to be up-to-date
  - Only maintainers can merge

---

## Testing Pipeline

### Pre-commit Hooks (Local)

Before committing, pre-commit hooks run automatically:

```bash
# These run on every commit:
1. Run Haskell Lisp-only Tests  → stack test --test-arguments="--only-lisp"
2. Clean Up Project             → stack clean, remove build artifacts
```

To run manually:
```bash
pre-commit run --all-files
pre-commit run run-lisp-tests --all-files
```

### CI Workflow on Pull Request

When you create or update a PR, GitHub Actions runs (`.github/workflows/ci.yml`):

1. **Build** — Compile the project
   - Checks for compilation errors
   - Runs on all branches

2. **Tests** — Run all test suites
   - Lisp tests (`.scm` files with expected outputs)
   - Parser tests (unit tests for combinators)
   - If tests fail, PR cannot be merged

3. **Code Quality** (if configured)
   - Linting, formatting checks
   - Static analysis

### Release Workflow on Merge

When a PR is **merged** to `develop` or `main`, GitHub Actions runs (`.github/workflows/release.yml`):

1. **Run Tests** (second verification)
   - Ensures tests still pass after merge
   - Prevents releasing broken code

2. **Version Bump** (if tests pass)
   - **Minor bump** on `develop` (e.g., `0.1.0` → `0.2.0`)
   - **Major bump** on `main` (e.g., `0.2.0` → `1.0.0`)
   - Updates `package.yaml` with new version

3. **Create Release**
   - Creates git tag (e.g., `v0.2.0`)
   - Creates GitHub Release with:
     - Release notes
     - Link to commits since last version
     - Download links (if artifacts attached)

---

## Release System

### Semantic Versioning

GLaDOS uses **Semantic Versioning**: `MAJOR.MINOR.PATCH`

Example: `1.2.3`

- **MAJOR** (e.g., `1.0.0`) — Breaking changes, major features
  - Bumped when merging to `main`
  - Indicates significant evolution of the project

- **MINOR** (e.g., `1.2.0`) — New features, improvements
  - Bumped when merging to `develop`
  - Backwards compatible

- **PATCH** (e.g., `1.2.3`) — Bug fixes
  - Defaults to `0` on automated releases
  - Can be manually incremented if needed

### Release Process Flow

```
┌─────────────────────────────────────────────────────────────┐
│ Developer creates PR on feature branch → develop            │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ CI Pipeline Runs                                            │
│ - Build project                                             │
│ - Run all tests                                             │
│ - Check code quality                                        │
└─────────────────────────────────────────────────────────────┘
                          ↓
                    PR Approved
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ PR Merged to develop                                        │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ Release Workflow Triggered                                  │
│ 1. Run tests again (final check)                            │
│ 2. Bump MINOR version (e.g., 0.1.0 → 0.2.0)               │
│ 3. Create git tag (v0.2.0)                                  │
│ 4. Create GitHub Release                                    │
│ 5. Commit version to develop                                │
└─────────────────────────────────────────────────────────────┘
                          ↓
        [Later, when ready for production]
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ Create PR from develop → main                               │
│ (Requires 3 approvals)                                      │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ PR Merged to main                                           │
└─────────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────────┐
│ Release Workflow Triggered                                  │
│ 1. Run tests again (final check)                            │
│ 2. Bump MAJOR version (e.g., 0.2.0 → 1.0.0)               │
│ 3. Create git tag (v1.0.0)                                  │
│ 4. Create GitHub Release (Major Release)                    │
│ 5. Commit version to main                                   │
└─────────────────────────────────────────────────────────────┘
```

### Example Release Timeline

```
Day 1: Feature PR merged to develop
  → v0.1.0 → v0.2.0 (Minor Release)
  → Released as v0.2.0

Day 5: Another feature merged to develop
  → v0.2.0 → v0.3.0 (Minor Release)
  → Released as v0.3.0

Day 10: Multiple features merged to develop, ready for production
  → Create PR: develop → main
  → Reviewed and approved by 3 people
  → PR merged to main
  → v0.3.0 → v1.0.0 (Major Release)
  → Released as v1.0.0 (Production)
```

### Accessing Releases

All releases are available at:
- GitHub Releases: https://github.com/ColAntoine/Glados/releases
- Git tags: `git tag -l` (list), `git checkout v1.0.0` (checkout)

---

## GitHub Actions Workflows

### 1. CI Workflow (`.github/workflows/ci.yml`)

**Triggers:** On push to any branch or PR

**Jobs:**
- `build` — Compiles the project
- `test` — Runs all tests

**Status:** Visible in PR checks

### 2. Release Workflow (`.github/workflows/release.yml`)

**Triggers:** On PR merge to `develop` or `main`

**Preconditions:**
- PR must be successfully merged
- Tests must pass

**Jobs:**
- `test` — Run all tests (final verification)
- `release` — Bump version, create tag, create GitHub Release

**Result:**
- New version tagged in git
- GitHub Release created
- Automatic commit with version bump

### Workflow Status

View workflow runs at:
- **Settings** → **Actions** → **All workflows**
- Or in each PR under **Checks** tab

---

## Best Practices

1. **Always work on feature branches** — Never commit directly to `develop` or `main`
2. **Write tests for new features** — Helps catch bugs early
3. **Use clear commit messages** — Helps with release notes and debugging
4. **Keep commits atomic** — One logical change per commit
5. **Test locally before pushing** — Run `stack test` before pushing
6. **Review PRs thoroughly** — Especially before approving merges to `main`
7. **Keep branches up-to-date** — Rebase on `develop` before merging

---

## Troubleshooting

### Tests Fail on CI but Pass Locally

- **Ensure local environment matches CI** (same GHC, Stack versions)
- **Check platform differences** (Windows vs Linux vs macOS)
- **Run `stack clean && stack build && stack test`** locally to rebuild from scratch

### PR Won't Merge Despite Passing Tests

- **Check branch protection rules** (Settings → Branches)
- **Ensure 3 approvals** for `main` branch
- **Check that branch is up-to-date** with target branch

### Release Didn't Trigger After Merge

- **Check if PR was actually merged** (not closed without merge)
- **Check workflow status** in Actions tab
- **Verify branch is `develop` or `main`** (not a different branch)
- **Check `GITHUB_TOKEN` permissions** in workflow (should be auto-configured)

---

## Questions?

For more information, see:
- [CONTRIBUTING.md](CONTRIBUTING.md) — Contribution guidelines
- [test/TESTING.md](test/TESTING.md) — Writing tests
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
