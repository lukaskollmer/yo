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

View on GitHub: [lukaskollmer/yo](https://github.com/lukaskollmer/yo)


## Table of Contents
- [Syntax / Language Features](#syntax--language-features)
- [Types](#types)
- [Memory Management](#memory-management)
- [Standard Library](#standard-library)
- [Roadmap](#roadmap)



## Syntax / Language Features
> **Note**: This list of features is incomplete, have a look at the [standard library](https://github.com/lukaskollmer/yo/tree/master/stdlib) to see the language in use.  
The full syntax is defined in [grammar.h](https://github.com/lukaskollmer/yo/blob/master/yo/Parser/grammar.h)

### Functions
Functions are declared using the `fn` keyword. Every function is required to specify a return type. Parameters are declared with a `name: type` syntax:

```rust
// The main function is the program's entry point
fn main(): int {
    return 0;
}

// A function that takes and returns an integer
fn increment(x: int): int {
    return x + 1;
}
```

yo differentiates between three kinds of functions:
- global functions: not bound to any specific namespace (like `main` and `increment` above)
- static functions: bound to a type's namespace (like `io::print`, see below)
- instance methods: called on an instance of a type (like `array.count()`)



### Literals
- String literals: `"Hello!"`
- Integer literals: `18` (base 10), `0b10010` (base 2), `0x12` (base 16), `022` (base 8)
- Double literals: `7.5`
- Array literals (complex): `[expr, ...]` (arrays can only contain reference-counted objects)
- Array literals (primitive): `{expr, ...}` (returns `*i64`)
- Number literals: `@5`, `@5.0`, `@(<expr>)`



### Variables
- Use the `let` keyword to declare a variable
- If a variable has an initial value, you can omit explicitly specifying a type

```rust
fn main(): int {
    let foo: String;

    let magicNumber = 42;
    let one = 1;
    let two = one * 2;

    return magicNumber + two;
}
```

**Builtin identifiers:**
- `#function`: evaluates to the signature of the current function

**Other kinds of variables**
- Static variables: declared globally and valid for the entire lifetime of the program
- Constants: compile-time constants, limited to trivial types

**Note**
- For both static and const variables, you're required to explicitly declare a type
- Only static variables of ARC-enabled types are deallocated automatically, you have to manually free other non-primitive static variables
- Constants behave similar to macros in C, as in that they're "pasted" into the ast whereever they're used

```rust
static numbers: *i64 = {1, 2, 4, 8, 16};

const EXIT_SUCCESS: i32 = 0;
```



### Conditional Statements
- Yo has support for `if`, `while` and two types of `for` statements
- Curly braces are always required
- C-style for-loops: `for <decl>; <cond>; <stmt>; { ... }`
- Iterating for-loops: `for <ident> in <iterable> { ... }`



### Importing other modules
Import other modules using the `use` keyword. Since yo doesn't have a concept of namespaces, all imported types and functions will be available globally (similar to C).

```rust
use "runtime";
use "std/array";

fn main(): int {
    let foo = Array::new();
}
```


### Casting
An expression can be cast to another type with the `as` keyword. In most cases, the cast doesn't modify the value, with the exception of casting between ingeters and doubles.




### Enums
Define an enum like the following:
```rust
enum Name {
    case1, case2, ...
}
```
For the time being, enums are implemented as integers, meaning that you can trivially pass them anywhere an integer is expected, simply by casting to `int`.




### Lambdas
Anonymous functions (lambdas) are declared using the `|| -> {}` syntax:

```rust
fn main(): int {
    let array = ["i", "am", "the", "doctor"];
    array.forEach(|obj: String| -> {
        io::print(obj);
    });
}
```

Unter the hood, lambdas are objects: they can be assigned to variables, passed to functions and stored as properties. A lambda type can be defined using the `fn<(param_types): return_type>` syntax.

For example, this is the signature of the `Array.sort` method:
```rust
fn sort(self: Array, f: fn<(id, id): bool>): void;
```

Lambdas can reference objects from outside their own scope, in which case the lambda will hold a strong reference to the object (this can lead to retain cycles!).  
Explicitly defining parameter types in a lambda isn't necssary, by default the parameters from the signature will be used:
```rust
// `x` and `y` are inferred to be of type int
let f1: fn<(int, int): int> = |x, y| -> { return x + y; };

// `x` is inferred to be of type int, while `y` is explicitly defined as a String
let f2: fn<(int, id): void> = |x, y: String| -> { ... };
```



### defer
The `defer` statement allows delaying the execution of some code until just before the current scope is exited.

For example, the following code will first print `foo`, and then `defer`:
```rust
fn foo(x: int): int {
    io::print("foo");
    return x;
}

fn main(): int {
    defer {
        io::print("defer");
    }
    return foo(0);
}
```


### Annotations

Type- and function declarations can be annotated using the `#[name]` syntax.  
A list of all annotations can be found in [ASTAnnotation.swift](https://github.com/lukaskollmer/yo/blob/master/yo/AST/ASTAnnotation.swift)

An incomplete list of annotations:

| Annotation           | Scope           | Description                                                       |
| :------------------- | :-------------- | :---------------------------------------------------------------- |
| `disable_arc`        | function        | Disable automatic reference counting for a function               |
| `disable_metadata`   | type            | Disable runtime metadata. This also disables ARC fot that type    |
| `static_initializer` | function        | The annotated function will be called before `main` is invoked    |
| `static_cleanup`     | function        | The annotated function will be called after returning from `main` |
| `variadic`           | function        | Tells the compiler to pack all non-fixed arguments into an array  |
| `unchecked`          | function, const | Disables type checking                                            |






### Variadic functions
The `#[variadic]` annotation indicates that a function accepts a variable number of parameters. How the variadic arguments are passed depends on the type of the last parameter:
- `Array`: all additional arguments must be of type `id` (ie, complex objects)
- `*i64`: all additional arguments must fit in a 64-bit integer

```rust
#[variadic]
fn format(fmt: String, args: Array): String;

#[variadic]
fn format(fmt: String, args: *i64): String;
```



### Working with pointers
You can use the `*`, `&+` and `&++` operators to reference or dereference values:
- `* <lvalue>` dereference an lvalue of pointer-type. equivalent to `<lvalue>[0]`
- `&+ <lvalue>` take the address of an lvalue
- `&++ <lvalue>` take the absolute address of an lvalue (only useful when passing pointers to the ffi)

```rust
fn increment_in_place(x: *int): void {
  *x += 1;
}

fn main(): int {
  let foo = 0;
  increment_in_place(&+foo);
  return foo;
} // -> Exits with exit code 1
```



### Boxing primitive values
Since yo doesn't support polymorphism or generics, and the standard library has to support ARC, most types and functions in the standard library accept objects of type `id` (aka, a pointer to a reference-counted object).

An obvous limitation of this is that you can't trivially store a value of primitive type in an `Array` or `HashMap`. yo works around this by providing a `Number` type which functions as a wrapper around `int`, `bool` or `double` values.  
The `@` operator constructs a `Number` object from a primitive literal or an expression which evaluates to a primitive type:
```rust
// Error: type mismatch
let obj: id = 5;

// Works fine, since `@5` evaluates to a `Number` object
let obj: id = @5;
```



### IO

The `io` module defines some functions for printing strings (`io::print`), ints (`io::printi`) or doubles (`io::printd`) to stdout.  

The `io` module also defines the `io::printf` function, which is yo's equivalent of C's `printf`. Note that you have to box primitive values passed to `io::printf`.

`io::printf` format specifiers:

| Specifier | Output                                 |
| :-------: | :------------------------------------- |
| `%n`      | A `Number` object's value              |
| `%s`      | The contents of a `String` object      |
| `%i`      | The integer value of the passed object |




## Types

yo defines the following primitive types:
- `bool`: A boolean type, implemented as an 8-bit integer. The only valid values are `true` and `false`
- `i8`, `i16`, `i32`, `i64`: Signed integers (`int` also exists as a type alias for `i64`)
- `double`: A double-precision floating-point type
- `void`

Additional special-purpose types:
- `any`: An unspecified type of the same size as `int`
- `id`: A reference-counted pointer

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
    let self = runtime::alloc(24) as Person;

    self[0] = runtime_type_metadata; // resolved at compile-time
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
    static fn me(): Person {
        return Person::init("Lukas", 20);
    }


    // Instance methods have to take `self` as their first parameter
    fn incrementAge(self: Person): void {
        self.age += 1;
    }
}
```

If you want a namespace to group some related functions, you can declare an `impl` block for a nonexistent type and put your functions in there (That's how the [io](https://github.com/lukaskollmer/yo/blob/master/stdlib/io/main.yo) module is implemented).





## Memory Management
Yo differentiates between stack-allocated primitive types and heap-allocated complex types.

- `runtime::alloc(num_bytes: int): *int` allocates space on the heap and returns a pointer to the beginning of the allocated data
- Accessing the data referenced by a pointer is possible by subscripting (see the example below)
- `runtime::free(ptr: any): void` frees the allocated heap space

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

**Note:** You only need to free data you allocated. Objects that use ARC are freed automatically



### A structs' default memory layout
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
| 8      | Pointer to `name` (a heap-allocated string)  |
| 16     | Value of `age`                               |

The first field contains the object's retain count (in the lower 4 bytes) and a pointer to the object's [metatype](https://github.com/lukaskollmer/yo/blob/master/stdlib/runtime/metatypes.yo) (the upper 4 bytes).



### Automatic Reference Counting
By default, automatic reference counting is enabled for all structs.  
You can disable ARC for a specific type with the `#[disable_metadata]` annotation. Note that this only works as long as the object's static type remains unchanged. If the static type decays to `id`, the compiler and runtime have no way of knowing that the object doesn't support reference counting.

Every object has a reference count, which represents the current number of references to that object. The runtime provides two functions for manipulating an object's reference count:
- `runtime::retain(obj: id): id` increases it by 1, indicating that a new reference to `obj` was created
- `runtime::release(obj: id): id` decreases it by 1, indicating that an existing reference to `obj` was destroyed

When an object's reference count reaches 0, the runtime calls its `dealloc` method and frees it off the heap.

The compiler generates a dealloc method for each type, which releases all references held by the object, but types can also implement their own `dealloc` method.





## Standard Library
The standard library is required for any yo program to compile. It implements fundamental types and data structures, like `String`, `Array`, `HashMap` and some others.  
The stdlib sources can be found [here](https://github.com/lukaskollmer/yo/tree/master/stdlib).






## FFI
yo's standard library includes an experimental ffi, which allows calling C functions from yo:

```rust
use "runtime";
use "io";
use "ffi";

fn main(): int {
    let ffi = FFI::new(nil);
    ffi.declareFunction("strcmp", FFIType.int32, 2, FFIType.pointer, FFIType.pointer);

    let foo = "foo";
    let bar = "bar";

    io::printf("same: %n", @(ffi.invoke("strcmp", &++foo._backing, &++foo._backing))); // -> "0"
    io::printf("same: %n", @(ffi.invoke("strcmp", &++foo._backing, &++bar._backing))); // -> "4"
}
```

Since yo strings internally use the same memory layout as C strings, they can be trivially passed back and forth.

Note that this is extremely limited for now, basically it's just passing around integers of arbitrary sizes, the ffi doesn't (yet?) support structs or c-style arrays.



## Roadmap

In the future, i'd like to add:
- OCaml-style pattern matching
- Optionals
- Dramatically improved parsing performance
- Variant types (this could be implemented as enums with associated values)
- Proper protocol support
- An Optimizer


## License
MIT @ [Lukas Kollmer](https://lukaskollmer.me)
