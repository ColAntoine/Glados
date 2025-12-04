// Example showing intended list-map usage (requires lists implementation)
fn square(x) = x * x

[1,2,3,4]
    |> map(square)
    |> print
