// Division assignment sugar: /=
// x /= y desugars to: x = x / y

fn divAssign(x, y) = {
    let x /= y
    x
}

divAssign(50, 5) |> print
