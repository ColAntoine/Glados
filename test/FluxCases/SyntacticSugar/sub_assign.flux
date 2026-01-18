// Subtraction assignment sugar: -=
// x -= y desugars to: x = x - y

fn subAssign(x, y) = {
    let x -= y
    x
}

subAssign(20, 7) |> print
