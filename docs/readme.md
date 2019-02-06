# yo
> the yo programming language

<br>

Yo is an interpreted, statically typed programming language with a rust-inspired syntax.

```rust
use "runtime";
use "io";

fn main(): int {
    io::print("Hello World!");
}
```




## Table of Contents
- [Syntax](#syntax)
- [Types](#types)
- [Memory Management](#memory-management)
- [Standard Library](stdlib/)

## Syntax
yo's syntax is heavily inspired by rust

> **Note**: This list of syntax features is most likely incomplete, have a look at the [standard library](https://github.com/lukaskollmer/yo/tree/master/stdlib) to see the language in use

### Functions
- Declare a function using the `fn` keyword
- Every function is required to specify a return type
- If the function body doesn't end with a return statement, it implicitly returns 0
- Return from a function using the `return` keyword
- Function parameters are declared using a `name: type` syntax
- When calling a function, primititve types are passed by value, while complex types ([see below](#types)) are passed by reference

```rust
// A function that takes 0 parameters and returns an integer
fn main(): int {
    return 12;
}

// A function that takes a single integer
fn addOne(x: int): int {
    return x + 1;
}
```

### Variables
- Use the `let` keyword to declare a variable
- If a variable has an initial value, you can omit explicitly specifying a type

```rust
fn main(): int {
    let magicNumber = 42;
    let one = 1;
    let two = one * 2;

    return magicNumber + two;
}
```

### Conditional Statements
- Yo has support for `if`, `while` and two types of`for` statements
- Curly braces are always required

```rust
fn abs(x: int): int {
    if x > 0 {
        return x;
    } else {
        return -x;
    }
}
```

### Importing other modules
Import other modules using the `use` keyword. Since yo doesn't have a concept of namespaces, all imported types and functions will be available globally (similar to C).

```rust
use "runtime";
use "std/array";

fn main(): int {
    let foo = Array::new();
}
```

### Lambdas
Anonymous functions (lambdas) can be declared using the `|| -> {}` syntax:

```rust
fn main(): int {
    let array = ["i", "am", "the", "doctor"];
    array.forEach(|obj: String| -> {
        io::print(obj);
    });
}
```

Unter the hood, lambdas are objects: they can be assigned to variables, passed to functions and stored as properties. A lambda type can be defined using the `fn<(param_types): return_type>` syntax. For example, this is the signature of the `Array.forEach` method from above:

```rust
fn forEach(self: Array, f: fn<(id): void>): void
```

Lambdas can reference objects from outside their own scope, in which case the lambda will hold a strong reference to the object (this can lead to retain cycles!)

### `defer`
The `defer` statement allows delaying the execution of some code until just before the current scope is exited.

For example, the following code will first print "foo", and then "defer":
```rust
fn foo(x: int): int {
    io::("foo");
    return x;
}

fn main(): int {
    defer {
        io::print("defer");
    }
    return foo(0);
}
```

## Types

yo defines the following primitive types:
- `bool`: A boolean type, implemented as an 8-bit integer. The only valid values are `true` and `false`
- `.i8`, `.i16`, `.i32`, `.i64`: Signed integers (`.int` also exists as a type alias for `.i64`)
- `.double`: A double-precision floating-point type
- `.void`

Additional special-purpose types:
- `.any`: An unspecified type of the same size as `.int`
- `.id`: A reference-counted pointer

Declare a type using the `struct` keyword:

```rust
struct Person {
    name: String,
    age: int
}
```

Instances of structs are allocated on the heap and are subject to automatic reference counting (see the [memory management](#memory-management) section). Reference counting can optionally be disabled on a per-type basis, as well as on a per-function basis.

The yo compiler automatically creates an initializer for every type. For example, if you declare the type `Person` as seen in the example above, yo synthesises the following initializer:
```rust
static fn init(name: String, age: int): Person {
    // If arc is enabled, the first field contains metadata about the object
    let self = runtime::alloc(8) as Person;
    self[1] = runtime::retain(name);
    self[2] = age;

    return self;
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
    fn incrementAge(self: Person): void {
        self.age += 1;
    }
}
```

If you want a namespace to group some related functions, you can declare an `impl` block for a nonexistent type and put your functions in there (That's how the [runtime](https://github.com/lukaskollmer/yo/blob/master/stdlib/std/runtime.yo) module is implemented).


## Memory Management
Yo differentiates between stack-allocated primitive types and heap-allocated complex types.

- `runtime::alloc(num_bytes: int): *int` allocates space on the heap and returns a pointer to the beginning of the allocated data
- Accessing the data referenced by a pointer is possible via the subscript (see the example below)
- `runtime::free(ptr: any)` frees the allocated heap space

**Example:** Allocating a C-String on the heap
```rust
fn main(): int {
    let string = runtime::alloc(5) as *i8;
    string[0] = 'T';
    string[1] = 'e';
    string[2] = 's';
    string[3] = 't';
    string[4] = 0;
    runtime::free(string);
}
```

**Note:** You only need to free data you allocated. Objects that use ARC are freed auttomatically


### A `structs`'s default memory layout
As described above, yo auto-generated an initializer for every struct type you declare. Let's say we declare `Person`, as defined above:

```rust
struct Person {
    name: String,
    age: int
}
```

The memory layout of an instance of `Person` is the following:

| Offset | Description                                  |
| :----- | :------------------------------------------- |
| 0      | Runtime metadata                             |
| 1      | Pointer to `name` (a heap-allocated string)  |
| 2      | Value of `age`                               |

The first field contains the object's retain count (in the lower 4 bytes) and a pointer to the object's [metatype](https://github.com/lukaskollmer/yo/blob/master/stdlib/runtime/metatypes.yo) (the upper 4 bytes).


### Automatic Reference Counting
By default, automatic reference counting is enabled for all structs. You can disable ARC for a specific type via the `#[disable_metadata]` annotation.

Every object has a reference count, which represents the current number of references to that object. The runtime provides 2 functions for changing an object's reference count:
- `runtime::retain(obj: id)` increases it by 1, indicating that a new reference to `obj` was created
- `runtime::retain(obj: id)` decreases it by 1, indicating that an existing reference to `obj` has ended

When an object's reference count reaches 0, the runtime calls its `dealloc` method and frees it off the heap.

The compiler generates a dealloc method for each type, which releases all references held by the object, but types can also implement their own `dealloc` method.


## License
MIT @ [Lukas Kollmer](https://lukaskollmer.me)
