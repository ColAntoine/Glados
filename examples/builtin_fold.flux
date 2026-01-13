// Test builtin: fold (reduce a list)

fn add(a, b) = a + b
fn mul(a, b) = a * b

print("=== Sum with fold ===")
fold(add, 0, [1, 2, 3, 4, 5]) |> print

print("=== Product with fold ===")
fold(mul, 1, [2, 3, 4]) |> print

print("=== Fold with subtraction ===")
fold((a, b) => a - b, 100, [10, 5, 3]) |> print
