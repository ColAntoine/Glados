// BSQ: Find and mark the biggest square

// Get character at 2D position (row, col) in string grid
fn getCharAt(content, row, col, width) =
    at(content, row * (width + 1) + col)

// Check if character is a dot
fn isDot(c) = c == "."

// Replace character at position in string
fn replaceAt(str, pos, newChar) = {
    let before = substring(str, 0, pos)
    let after = substring(str, pos + 1, len(str))
    concat(concat(before, newChar), after)
}

// Check if square of given size fits at (row, col)
fn checkSquareCell(content, r, c, endR, endC, startC, width) = {
    if r >= endR {
        1
    } else {
        if c >= endC {
            checkSquareCell(content, r + 1, startC, endR, endC, startC, width)
        } else {
            let ch = getCharAt(content, r, c, width)
            if isDot(ch) {
                checkSquareCell(content, r, c + 1, endR, endC, startC, width)
            } else {
                0
            }
        }
    }
}

fn canFitSquare(content, row, col, size, width, rows) = {
    if row + size > rows {
        0
    } else {
        if col + size > width {
            0
        } else {
            checkSquareCell(content, row, col, row + size, col + size, col, width)
        }
    }
}

// Scan for a square of given size
fn scanForSize(content, row, col, size, width, rows) = {
    if row >= rows {
        0
    } else {
        if col >= width {
            scanForSize(content, row + 1, 0, size, width, rows)
        } else {
            let fits = canFitSquare(content, row, col, size, width, rows)
            if fits == 1 {
                row * 10000 + col * 100 + size
            } else {
                scanForSize(content, row, col + 1, size, width, rows)
            }
        }
    }
}

// Try progressively smaller sizes to find biggest
fn findBiggest(content, size, width, rows) = {
    if size < 1 {
        0
    } else {
        let result = scanForSize(content, 0, 0, size, width, rows)
        if result > 0 {
            result
        } else {
            findBiggest(content, size - 1, width, rows)
        }
    }
}

// Mark one cell in the square
fn markSquareCell(result, row, col, endR, endC, startC, width) = {
    if row >= endR {
        result
    } else {
        if col >= endC {
            markSquareCell(result, row + 1, startC, endR, endC, startC, width)
        } else {
            let pos = row * (width + 1) + col
            let newResult = replaceAt(result, pos, "x")
            markSquareCell(newResult, row, col + 1, endR, endC, startC, width)
        }
    }
}

fn markSquare(content, startRow, startCol, size, width) =
    markSquareCell(content, startRow, startCol, startRow + size, startCol + size, startCol, width)

// Main solver
fn solveBSQ(mapPath) = {
    let content = readFile(mapPath)
    let rows = 21
    let width = 97

    let d1 = print("Solving BSQ...")

    let result = findBiggest(content, min(rows, width), width, rows)
    let size = result - (result / 100) * 100
    let col = (result / 100) - (result / 10000) * 100
    let row = result / 10000

    let d2 = print("Found square: size ")
    let d3 = print(size)
    let d4 = print(" at (")
    let d5 = print(row)
    let d6 = print(", ")
    let d7 = print(col)
    let d8 = print(")")

    let marked = markSquare(content, row, col, size, width)

    let d9 = writeFile("bsq_result.txt", marked)
    let d10 = print("Result written!")

    0
}

solveBSQ("map.txt")
