// local_bindings.flux
// Demonstrates storing variables inside a function using `let` and using them later

fn compute(n) = {
  // local vars inside the function
  let a = n + 1
  let b = a * 2
  // last expression is the return value
  b + a
}

// Call the function and print the result (expect 12 for input 3)
compute(3) |> print
