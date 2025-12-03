// Demo of new Flux syntax features

// 1. Returning function with = and native string type
fn greet(name) = "Hello, " // string support coming soon with concat

// 2. If/else with braces (no then/else keywords)
fn classify(n) = {
    if n > 0 {
        "positive"
    } else {
        "negative or zero"
    }
}

0 |> classify |> print
// 3. Tuple literals and unpacking via pipeline
fn add(a, b) = a + b

(5, 10) |> add |> print

// 4. Local let bindings in returning function
fn compute(x) = {
    let doubled = x * 2
    let squared = doubled * doubled
    squared + 1
}

compute(3) |> print

// 5. RET for explicit early return
fn findPositive(n) = {
    if n > 0 {
        RET n
    } else {
        RET 0
    }
}

findPositive(-5) |> print
findPositive(42) |> print
