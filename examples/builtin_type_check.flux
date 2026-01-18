// Test builtin: type checking (isInt, isBool, isString, isList)

print("=== isInt ===")
isInt(42) |> print
isInt("hello") |> print

print("=== isBool ===")
isBool(true) |> print
isBool(42) |> print

print("=== isString ===")
isString("hello") |> print
isString(42) |> print

print("=== isList ===")
isList([1, 2, 3]) |> print
isList(42) |> print
