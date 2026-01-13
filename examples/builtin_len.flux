// Test builtin: len (length of string and list)

print("=== String length ===")
len("hello") |> print
len("") |> print
len("Flux language") |> print

print("=== List length ===")
len([1, 2, 3, 4, 5]) |> print
len([]) |> print
len([10, 20, 30]) |> print
