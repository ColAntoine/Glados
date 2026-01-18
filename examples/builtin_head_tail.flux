// Test builtin: head and tail (list operations)

print("=== head (first element) ===")
head([1, 2, 3, 4, 5]) |> print
head("Hello") |> print

print("=== tail (rest of elements) ===")
tail([1, 2, 3, 4, 5]) |> print
tail("Hello") |> print
