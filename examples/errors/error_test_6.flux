// error_test_6.flux
// ERROR: Two functions with the same name

fn test(x) = x + 1
fn test(y) = y * 2

test(5) |> print
