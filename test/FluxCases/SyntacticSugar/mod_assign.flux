// Modulo assignment sugar: %=
// x %= y desugars to: x = x % y

fn modAssign(x, y) = {
    let x %= y
    x
}

modAssign(17, 5) |> print
