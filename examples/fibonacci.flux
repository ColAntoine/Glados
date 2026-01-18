// fibonacci.flux
// Recursive Fibonacci implementation

fn fib(n) = {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

// Calculate Fibonacci numbers 0 through 10
fn printFibSequence(current, max) = {
    if current <= max {
        fib(current) |> print
        printFibSequence(current + 1, max)
    } else {
        0
    }
}

printFibSequence(0, 10)
