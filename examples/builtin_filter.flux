// Test builtin: filter (filter list with predicate)

fn isEven(n) = n % 2 == 0
fn isPositive(n) = n > 0
fn greaterThan10(n) = n > 10

print("=== Filter even numbers ===")
filter(isEven, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) |> print

print("=== Filter positive numbers ===")
filter(isPositive, [-5, -2, 0, 3, 8, -1, 10]) |> print

print("=== Filter numbers > 10 ===")
filter(greaterThan10, [5, 12, 8, 15, 3, 20]) |> print
