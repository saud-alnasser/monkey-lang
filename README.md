## About ❓

A programming language written in Rust, following the book "Writing an Interpreter in Go" by Thorsten Ball.

---

## Language Specification

### Overview

Monkey is a dynamically-typed, interpreted programming language with support for:

- First-class functions and closures
- Dynamic typing with multiple data types
- Arrays and hash objects
- Control flow statements (if/else)
- Variable binding and scoping
- Built-in functions for common operations

### Data Types

Monkey supports the following data types:

| Type          | Description                   | Example                       |
| ------------- | ----------------------------- | ----------------------------- |
| **Integer**   | 64-bit signed integers        | `5`, `-10`, `42`              |
| **String**    | UTF-8 encoded strings         | `"hello"`, `"world"`          |
| **Boolean**   | Boolean values                | `true`, `false`               |
| **Array**     | Ordered collection of values  | `[1, 2, 3]`, `["a", "b"]`     |
| **Object**    | Hash map with key-value pairs | `{"name": "John", "age": 30}` |
| **Function**  | First-class function objects  | `fn(x) { x + 1 }`             |
| **Null**      | Null value                    | `null`                        |
| **Undefined** | Undefined/void value          | (implicit return value)       |

### Operators

#### Arithmetic Operators

| Operator | Description                                    | Example                                               |
| -------- | ---------------------------------------------- | ----------------------------------------------------- |
| `+`      | Addition (integers) or concatenation (strings) | `5 + 3` → `8`, `"hello" + " world"` → `"hello world"` |
| `-`      | Subtraction                                    | `10 - 3` → `7`                                        |
| `*`      | Multiplication                                 | `4 * 5` → `20`                                        |
| `/`      | Integer division                               | `10 / 3` → `3`                                        |

#### Comparison Operators

| Operator | Description           | Example           |
| -------- | --------------------- | ----------------- |
| `==`     | Equality              | `5 == 5` → `true` |
| `!=`     | Inequality            | `5 != 3` → `true` |
| `<`      | Less than             | `3 < 5` → `true`  |
| `>`      | Greater than          | `5 > 3` → `true`  |
| `<=`     | Less than or equal    | `3 <= 3` → `true` |
| `>=`     | Greater than or equal | `5 >= 3` → `true` |

#### Logical Operators

| Operator | Description | Example                           |
| -------- | ----------- | --------------------------------- |
| `!`      | Logical NOT | `!true` → `false`, `!5` → `false` |

#### Prefix Operators

| Operator | Description | Example           |
| -------- | ----------- | ----------------- |
| `-`      | Negation    | `-5` → `-5`       |
| `!`      | Logical NOT | `!true` → `false` |

### Statements

#### Variable Declaration

Declare and bind variables using the `let` keyword:

```monkey
let x = 5;
let name = "Alice";
let arr = [1, 2, 3];
```

#### Return Statement

Return a value from a function or program:

```monkey
return 42;
return x + y;
```

#### Expression Statement

Any expression can be used as a statement:

```monkey
5 + 5;
fn(x) { x * 2; };
```

### Expressions

#### Literals

**Integer Literals:**

```monkey
5
-10
42
```

**String Literals:**

```monkey
"hello"
"world"
```

**Boolean Literals:**

```monkey
true
false
```

**Array Literals:**

```monkey
[]
[1, 2, 3]
[1 + 2, 3 * 4, 5 - 6]
["a", "b", "c"]
```

**Object Literals:**

```monkey
{}
{"name": "John", "age": 30}
{1: "one", 2: "two"}
{true: "yes", false: "no"}
```

#### Identifier

Reference a variable or function:

```monkey
x
name
myFunction
```

#### Infix Expressions

Binary operations with two operands:

```monkey
5 + 3
10 - 2
4 * 5
10 / 2
5 < 10
5 > 3
5 == 5
5 != 3
```

#### Prefix Expressions

Unary operations with one operand:

```monkey
-5
!true
!!false
```

#### If-Else Expressions

Conditional execution:

```monkey
if (x > 5) {
    10
} else {
    20
}
```

Truthiness rules:

- `true` is truthy
- `false` is falsy
- Non-zero integers are truthy
- Zero is falsy
- Other values are falsy

#### Function Expressions

Define anonymous functions:

```monkey
fn(x) { x + 1 }
fn(x, y) { x + y }
fn() { 42 }
```

Functions are first-class values and can be:

- Assigned to variables
- Passed as arguments
- Returned from other functions
- Used in closures

#### Function Calls

Call a function with arguments:

```monkey
add(5, 3)
fn(x) { x * 2 }(5)
myFunc()
```

#### Index Expressions

Access elements in arrays or objects:

```monkey
[1, 2, 3][0]        // → 1
[1, 2, 3][1]        // → 2
arr[i]              // array indexing with variable
{"name": "John"}["name"]  // → "John"
{1: "one"}[1]       // → "one"
```

Out-of-bounds array access returns `null`.

#### Block Expressions

Group multiple statements:

```monkey
{
    let x = 5;
    let y = 10;
    x + y
}
```

### Scoping and Closures

Monkey supports lexical scoping with closures:

```monkey
let add = fn(x) {
    fn(y) {
        x + y
    }
};

let add_five = add(5);
add_five(3)  // → 8
```

Functions capture their enclosing environment, allowing them to access variables from outer scopes.

### Built-in Functions

Monkey provides the following built-in functions:

#### `len(value)`

Returns the length of a string or array.

```monkey
len("hello")      // → 5
len([1, 2, 3])    // → 3
len([])           // → 0
```

#### `first(array)`

Returns the first element of an array, or `null` if empty.

```monkey
first([1, 2, 3])  // → 1
first([])         // → null
```

#### `last(array)`

Returns the last element of an array, or `null` if empty.

```monkey
last([1, 2, 3])   // → 3
last([])          // → null
```

#### `rest(array)`

Returns all elements except the first, or an empty array if the input is empty.

```monkey
rest([1, 2, 3])   // → [2, 3]
rest([1])         // → []
rest([])          // → []
```

#### `push(array, value)`

Returns a new array with the value appended.

```monkey
push([1, 2], 3)   // → [1, 2, 3]
push([], 1)       // → [1]
```

#### `puts(...values)`

Prints values to standard output and returns `undefined`.

```monkey
puts("hello")
puts(1, 2, 3)
```

### Keywords

| Keyword  | Purpose                      |
| -------- | ---------------------------- |
| `let`    | Variable declaration         |
| `fn`     | Function definition          |
| `return` | Return from function/program |
| `if`     | Conditional execution        |
| `else`   | Alternative branch           |
| `true`   | Boolean true value           |
| `false`  | Boolean false value          |

### Syntax Examples

#### Variable Binding and Arithmetic

```monkey
let x = 5;
let y = 10;
x + y;  // → 15
```

#### Functions and Closures

```monkey
let multiply = fn(x, y) {
    x * y
};

multiply(3, 4);  // → 12

let make_adder = fn(x) {
    fn(y) {
        x + y
    }
};

let add_five = make_adder(5);
add_five(3);  // → 8
```

#### Arrays and Indexing

```monkey
let arr = [1, 2, 3, 4, 5];
arr[0];       // → 1
arr[2];       // → 3
arr[10];      // → null

let numbers = [1, 2, 3];
first(numbers);   // → 1
last(numbers);    // → 3
rest(numbers);    // → [2, 3]
```

#### Objects and Key Access

```monkey
let person = {"name": "Alice", "age": 30};
person["name"];   // → "Alice"
person["age"];    // → 30
person["email"];  // → null
```

#### Conditionals

```monkey
if (5 > 3) {
    "five is greater"
} else {
    "three is greater"
}

let max = fn(x, y) {
    if (x > y) {
        x
    } else {
        y
    }
};

max(10, 20);  // → 20
```

#### String Concatenation

```monkey
"Hello" + " " + "World"  // → "Hello World"
```

### Type System

Monkey is dynamically typed. Type checking occurs at runtime:

```monkey
5 + 5           // → 10 (integer arithmetic)
"5" + "5"       // → "55" (string concatenation)
5 + "5"         // → ERROR (type mismatch)
```

### Error Handling

Monkey provides error messages for:

- Type mismatches in operations
- Undefined identifiers
- Invalid function calls
- Out-of-bounds array access (returns `null`)
- Invalid arguments to built-in functions

---
