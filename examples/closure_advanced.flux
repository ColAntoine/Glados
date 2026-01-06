// Test nested closures and multiple captures

fn makeMultiplier(factor) = {
    (x) => x * factor
}

fn compose(f, g) = {
    (x) => f(g(x))
}

let double = makeMultiplier(2)
let triple = makeMultiplier(3)

print(double(5))   // Should print 10
print(triple(5))   // Should print 15

// Test nested capture
fn makeCounter(start) = {
    let count = start
    (increment) => count + increment
}

let counter = makeCounter(100)
print(counter(5))   // Should print 105
print(counter(10))  // Should print 110
