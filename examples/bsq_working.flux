// bsq_working.flux
// Working BSQ implementation

fn min3(a, b, c) = {
    if a <= b {
        if a <= c { a } else { c }
    } else {
        if b <= c { b } else { c }
    }
}

// Build zero row
fn buildZeroRow(cols, acc) = {
    if length(acc) >= cols {
        acc
    } else {
        buildZeroRow(cols, append(acc, 0))
    }
}

// Convert char to value
fn charVal(ch) = if ch == "." { 1 } else { 0 }

// Build first DP row
fn buildFirstRow(line, col, acc) = {
    if col >= length(line) {
        acc
    } else {
        buildFirstRow(line, col + 1, append(acc, charVal(charAt(line, col))))
    }
}

// Calculate DP cell value
fn calcCell(prevRow, currRow, origLine, col) = {
    if charAt(origLine, col) == "o" {
        0
    } else {
        if col == 0 {
            1
        } else {
            min3(get(prevRow, col), get(currRow, col - 1), get(prevRow, col - 1)) + 1
        }
    }
}

// Build DP row
fn buildRow(prevRow, origLine, col, cols, acc) = {
    if col >= cols {
        acc
    } else {
        buildRow(prevRow, origLine, col + 1, cols, append(acc, calcCell(prevRow, acc, origLine, col)))
    }
}

// Build DP matrix
fn buildMatrix(lines, dp, row, rows, cols) = {
    if row >= rows {
        dp
    } else {
        if row == 0 {
            buildMatrix(lines, append(dp, buildFirstRow(get(lines, 0), 0, [])), 1, rows, cols)
        } else {
            buildMatrix(lines, append(dp, buildRow(get(dp, row - 1), get(lines, row), 0, cols, [])), row + 1, rows, cols)
        }
    }
}

// Find max in row
fn maxInRow(row, col, cols, mVal, mCol) = {
    if col >= cols {
        [mVal, mCol]
    } else {
        if get(row, col) > mVal {
            maxInRow(row, col + 1, cols, get(row, col), col)
        } else {
            maxInRow(row, col + 1, cols, mVal, mCol)
        }
    }
}

// Find max in matrix
fn maxInMatrix(dp, row, rows, cols, mVal, mRow, mCol) = {
    if row >= rows {
        [mVal, mRow, mCol]
    } else {
        if get(maxInRow(get(dp, row), 0, cols, 0, 0), 0) > mVal {
            maxInMatrix(dp, row + 1, rows, cols, get(maxInRow(get(dp, row), 0, cols, 0, 0), 0), row, get(maxInRow(get(dp, row), 0, cols, 0, 0), 1))
        } else {
            maxInMatrix(dp, row + 1, rows, cols, mVal, mRow, mCol)
        }
    }
}

// Replace char at position
fn replaceAt(str, pos, ch) = {
    join([substring(str, 0, pos), ch, substring(str, pos + 1, length(str))], "")
}

// Mark line
fn markLine(line, col, size, cnt) = {
    if cnt >= size {
        line
    } else {
        markLine(replaceAt(line, col - cnt, "x"), col, size, cnt + 1)
    }
}

// Mark square
fn markSquare(lines, row, col, size, cnt, acc) = {
    if cnt >= size {
        acc
    } else {
        markSquare(lines, row, col, size, cnt + 1, append(acc, markLine(get(lines, row - cnt), col, size, 0)))
    }
}

// Rebuild lines
fn rebuild(lines, marked, before, after, idx, acc) = {
    if idx >= length(lines) {
        acc
    } else {
        if idx < before {
            rebuild(lines, marked, before, after, idx + 1, append(acc, get(lines, idx)))
        } else {
            if idx <= after {
                rebuild(lines, marked, before, after, idx + 1, append(acc, get(marked, idx - before)))
            } else {
                rebuild(lines, marked, before, after, idx + 1, append(acc, get(lines, idx)))
            }
        }
    }
}

// Print lines
fn printAll(lines, idx) {
    if idx < length(lines) {
        print(get(lines, idx))
        print("\n")
        printAll(lines, idx + 1)
    } else {
        0
    }
}

// Process BSQ result
fn processResult(lines, rows, cols) {
    let dp = buildMatrix(lines, [], 0, rows, cols)
    let max = maxInMatrix(dp, 0, rows, cols, 0, 0, 0)
    let size = get(max, 0)
    let row = get(max, 1)
    let col = get(max, 2)

    if size > 0 {
        let marked = markSquare(lines, row, col, size, 0, [])
        let result = rebuild(lines, marked, row - size + 1, row, 0, [])
        printAll(result, 0)
    } else {
        print("No square\n")
    }
}

// Main
fn bsq(file) {
    let content = readFile(file)
    let lines = readLines(file)
    let rows = length(lines)

    print("=== BSQ ===\n")
    print(content)
    print("\n")

    if rows > 0 {
        processResult(lines, rows, length(get(lines, 0)))
    } else {
        print("Empty\n")
    }
}

bsq("examples/map.txt")
