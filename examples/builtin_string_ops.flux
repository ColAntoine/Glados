// Test builtin: string operations (charAt, substring, toUpper, toLower)

print("=== charAt ===")
let str = "Flux"
charAt(str, 0) |> print
charAt(str, 2) |> print

print("=== substring ===")
let text = "Hello World"
substring(text, 0, 5) |> print
substring(text, 6, 11) |> print

print("=== toUpper ===")
toUpper("hello") |> print
toUpper("Flux Language") |> print

print("=== toLower ===")
toLower("HELLO") |> print
toLower("Flux Language") |> print
