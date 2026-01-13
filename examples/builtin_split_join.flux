// Test builtin: split and join

print("=== split ===")
split("Hello,World,Flux", ",") |> print
split("one two three", " ") |> print

print("=== join ===")
join(["Hello", "World", "Flux"], " ") |> print
join(["a", "b", "c"], ",") |> print
join(["one", "two"], "-") |> print
