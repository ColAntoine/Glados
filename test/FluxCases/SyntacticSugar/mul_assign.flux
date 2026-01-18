// Multiplication assignment sugar: *=
// x *= y desugars to: x = x * y

fn mulAssign(x, y) = {
    let x *= y
    x
}

mulAssign(4, 3) |> print
