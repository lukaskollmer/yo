# yo
> the yo programming language

<br>

Yo is an interpreted programming language with a rust-inspired syntax.

```rust
fn main(): int {
    io::print("hello world");
}
```

## Table of Contents
- [Syntax](#syntax)
- [Types](#types)
- [Memory Management](#memory-management)
- [Standard Library](stdlib/)

## Syntax

Every program is required to declare a `main` function, which is the program's entry point.

### Functions

- Declare a function using the `fn` keyword
- Every function is required to specify a return type
- If the function body doesn't end with a return statement, it implicitly returns 0
- Return from a function using the `ret` keyword
- Function parameters are declared using a `name: type` syntax
- When calling a function, integers are passed by value, while instances of types ([see below](#types)) are passed by reference

```rust
// A function that takes 0 parameters and returns an integer
fn main(): int {
    ret 12;
}

// A function that takes a single integer
fn addOne(x: int): int {
    ret x + 1;
}
```

### Variables
- Use the `val` keyword to declare a variable
- You always have to specify the variable's type (automatic type inference is planned)
- Variables are valid identifiers for the scope of the function they're declared in (hoisting)

```rust
fn main(): int {
    val magicNumber: int = 42;
    val one: int = 1;
    val two: int = one * 2;

    ret magicNumber + two;
}
```

### Conditional Statements
- Yo has support for `if` and `while` statements
- `for` loops are planned but not yet implemented
- Curly brackets are always required

```rust
fn abs(x: int): int {
    if x > 0 {
        ret x;
    } else {
        ret -x;
    }
}
```

### Importing other modules
Import other modules using the `use` keyword. Since yo doesn't have a concept of namespaces, all imported types and functions will be available globally. As of right now, you can only import standard library modules (relative imports are planned).
Some builtin functions are also available without explicitly importing them (ie `io::print`). If your code contains string literals, you have to import `std/string`.

```rust
use "std/array";

fn main(): int {
    val foo: Array = Array::new();
}
```


## Types
Declare a type using the `type` keyword:

```rust
type Person(name: String, age: int, alive: int);
```

Instances of types are allocated on the heap and can be subject to reference counting (see the [Memory Management](#memory-management) section).

The yo compiler automatically creates an initializer for every type. For example, if you declare the type `Person` as seen in the example above, yo synthesises the following initializer:
```rust
static fn init(name: String, age: int, alive: int): Person {
    val self: Person = runtime::alloc(4); // the first field contains runtime information, see the memory management section below
    self[1] = name;
    self[2] = age;
    self[3] = alive;

    ret self;
}
```

Call instance methods using the dot syntax (`foo.bar()`) and call static methods using the double-colon syntax (`Person::init()`).

You can declare a type's static and instance methods using the `impl` keyword. A single type can have multiple `impl` blocks.
```rust
impl Person {
    // Declare a static method using the `static` keyword
    static fn new(name: String, age: int): Person {
        ret Person::init(name, age, 1);
    }


    // Instance methods have to take `self` as their first parameter
    fn getAge(self: Person): int {
        ret self.age;
    }

    // Every type is required to have a dealloc function
    fn dealloc(self: Person): int {
        runtime::release(name);
    }
}
```

If you want a namespace to group some related functions, you can declare an `impl` block for a nonexistent type and put your functions in there (That's how the [runtime](/stdlib/std/runtime.yo) module is implemented).


## Memory Management
Yo differentiates between stack-allocated primitives (basically just integers) and heap-allocated data. You can put basically anything on the heap.

Use the `runtime::alloc(size: int)` function to allocate space on the heap. `runtime::alloc` returns an integer, which is a pointer to the beginning of the allocated data. Use the subscript synytax to read and write the data blob. You can free the allocated data using `runtime::free`. The pointer is now invalid.

**Example:** Allocating a blob of data on the heap
```rust
fn main(): int {
    val data: int = runtime::alloc(5);
    data[0] = 1;
    data[1] = 2;
    data[2] = 3;
    data[3] = 5;
    data[4] = 7;
    runtime::free(data);
}
```


### A `type`'s memory layout
As described above, yo auto-synthesises an initializer for every type you declare. Let's say we declare `Person`, as defined above:

```rust
type Person(name: String, age: int, alive: int);
```

The memory layout of an instance of `Person` is the following:

| Offset | Description                                       |
| :----- | :------------------------------------------------ |
| 0      | "Meta"-field w/ runtime data                      |
| 1      | Pointer to `name` (a heap-allocated string)       |
| 2      | Value of `age`                                    |
| 3      | Value of `alive`                                  |

The content of the first field is an integer, containing the following data:
| Bit Range | Contents                                 |
| :-------- | :--------------------------------------- |
| 59...40   | address of the type's `dealloc` function |
| 39...20   | the object's retain count                |
| 19...0    | the object's type                        |


### Reference Counting
Every object's initial reference count is 0. Call `runtime::retain` to increase an object's retain count by 1, call `runtime::release` to decrease it by 1. If an object's retain count reaches 0, the runtime automatically calls its `dealloc` function and frees it off the heap.

#### Automatic Reference Counting
I intend to implement clang-like automatic reference counting eventually.

As of right now, the only case where automatic `retain` and `release` calls are inserted is passing string literals to a function call.


## License
MIT @ [Lukas Kollmer](https://lukaskollmer.me)
