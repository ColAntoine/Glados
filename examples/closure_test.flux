// Test closure variable capture

fn makeAdder(x) = {
    (y) => x + y
}

let add5 = makeAdder(5)
let add10 = makeAdder(10)

print(add5(3))   // Should print 8
print(add10(3))  // Should print 13
print(add5(7))   // Should print 12
