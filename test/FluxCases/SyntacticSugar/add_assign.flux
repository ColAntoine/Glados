// Addition assignment sugar: +=
// x += y desugars to: x = x + y

fn addAssign(x, y) = {
    let x += y
    x
}

addAssign(10, 5) |> print
