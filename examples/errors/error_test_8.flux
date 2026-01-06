// error_test_8.flux
// ERROR: Function and variable with same name

fn myValue(x) = x * 2
let myValue = 42

myValue |> print
