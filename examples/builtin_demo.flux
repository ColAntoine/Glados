// Comprehensive builtin demo - showcasing all new features

print("=== STRING OPERATIONS ===\n")
let message = "Hello Flux Language"
print("Length:")
print(len(message))
print(toUpper(message))
print(toLower(message))
print(substring(message, 0, 5))
print(reverse(message))

print("\n=== LIST OPERATIONS ===\n")
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
print("Length:")
print(len(numbers))
print(head(numbers))
print(tail(numbers))
print(at(numbers, 4))
print(reverse(numbers))

print("\n=== MATH OPERATIONS ===\n")
print("abs(-42):")
print(abs(-42))
print("min(10, 20):")
print(min(10, 20))
print("max(10, 20):")
print(max(10, 20))
print("pow(2, 8):")
print(pow(2, 8))

print("\n=== FUNCTIONAL PROGRAMMING ===\n")
fn isEven(n) = n % 2 == 0
fn add(a, b) = a + b

let evens = filter(isEven, numbers)
print("Even numbers:")
print(evens)

let sum = fold(add, 0, numbers)
print("Sum:")
print(sum)

let doubled = map((x) => x * 2, range(5))
print("Doubled range:")
print(doubled)

print("\n=== TYPE CHECKING ===\n")
print("isInt(42):")
print(isInt(42))
print("isString(\"test\"):")
print(isString("test"))
print("isList([1,2,3]):")
print(isList([1, 2, 3]))

print("\n=== STRING SPLIT/JOIN ===\n")
let words = split(message, " ")
print("Words:")
print(words)
let joined = join(words, "-")
print("Joined:")
print(joined)

print("\nAll builtins working perfectly!\n")
