// Compilation demo - features that work with compiler

print("=== ARITHMETIC ===")
print(42)
print(10 + 20)
print(100 - 58)
print(6 * 7)

print("\n=== FUNCTIONS ===")
fn double(x) = x * 2
fn square(x) = x * x

print(double(21))
print(square(7))

print("\n=== CONDITIONALS ===")
fn max2(a, b) = if a > b then a else b
print(max2(15, 30))
print(max2(50, 25))

print("\n=== RECURSION ===")
fn factorial(n) = if n <= 1 then 1 else n * factorial(n - 1)
print(factorial(5))

fn fibonacci(n) = if n <= 1 then n else fibonacci(n - 1) + fibonacci(n - 2)
print(fibonacci(10))

print("\nCompilation successful! 🎉")
