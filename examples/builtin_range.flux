// Test builtin: range (generate list of numbers)

print("=== range(10) - 0 to 9 ===")
range(10) |> print

print("=== range(5, 15) - 5 to 14 ===")
range(5, 15) |> print

print("=== range(0, 5) ===")
range(0, 5) |> print

print("=== Use range with map ===")
fn double(x) = x * 2
map(double, range(5)) |> print
