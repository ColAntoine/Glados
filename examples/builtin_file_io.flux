// Test builtin: readFile, writeFile, appendFile

print("=== Read file ===\n")
readFile("examples/map.txt") |> print

print("=== Write file ===\n")
writeFile("examples/test_output.txt", "Hello from Flux!\n")
readFile("examples/test_output.txt") |> print

print("=== Append to file ===\n")
appendFile("examples/test_output.txt", "Appended line\n")
readFile("examples/test_output.txt") |> print
