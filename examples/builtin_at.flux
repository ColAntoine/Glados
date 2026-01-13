// Test builtin: at (get element at index)

print("=== List indexing ===\n")
let list = [10, 20, 30, 40, 50]
at(list, 0) |> print
at(list, 2) |> print
at(list, 4) |> print

print("=== String indexing ===")
let str = "Flux"
at(str, 0) |> print
at(str, 1) |> print
at(str, 3) |> print
print("\n")
