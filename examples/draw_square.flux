fn drawLine(n) {
    if n > 0 {
        print("*")
        drawLine(n - 1)
    } else {
        print("\n")
    }
}

fn drawSquare(n, max) {
    let line = max
    if n > 0 {
        drawLine(line)
        drawSquare(n - 1, max)
    } else {}
}

drawSquare(5, 5)