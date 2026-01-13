// Test builtin: concat (concatenate strings or lists)

print("=== String concatenation ===\n")
concat("Hello", " World\n") |> print
concat("Flux ", "Language\n") |> print

print("=== List concatenation ===\n")
concat([1, 2, 3], [4, 5, 6]) |> print
concat([10], [20, 30]) |> print
