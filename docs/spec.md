---
layout: default
title: Specification
---




> **NOTE:** This is very much still work in progress and does not necessarily describe the language as implemented in the <a class="casual-underlined" href="https://github.com/lukaskollmer/yo">GitHub repo</a>


<div id="toc"></div>


<h2 sectionId="yo.lex">Lexical structure</h2>
Valid Yo source code is written in ASCII. Some UTF-8 codepoints will probably work in identifiers and string literals, but there's no proper handling for characters outside the ASCII character set.


<h3 sectionId="yo.lex.comment">Comments</h3>
There are two kinds of comments:
- Line Comments, starting with `//` and continuing until the next line break (`\n`)
- Comment Blocks, starting with `/*` and continuing until the next `*/`. Comment blocks cannot be nested


<h3 sectionId="yo.lex.tokens">Tokens</h3>
The Yo lexer differentiates between the following kinds of tokens: keywords, identifiers, punctuation and literals.


<h3 sectionId="yo.lex.keyword">Keywords</h3>
Yo reserves the following keywords:
```
defer else fn for if impl in let mut match return struct use var while
```

<h3 sectionId="yo.lex.ident">Identifiers</h3>
An identifier is a sequence of one or more letters or digits. The first element must not be a digit.
```
digit  = [0-9]
letter = [a-zA-Z_]
ident  = <letter> (<letter>|<digit>)*
```
A sequence of characters that satisfies the `ident` pattern above and is not a reserved keyword is assumed to be an identifier.


<h3 sectionId="yo.lex.operators">Operators and punctuation</h3>

The following character sequences represent <a class="casual-underlined" href="#yo.expr.operators">operators</a> and punctuation:
```
+    &    &&    ==    |>    (    )
-    |    ||    !=    =     {    }
*    ^          <     !     [    ]
/    <<         <=          .    ;
%    >>         >           ,    :
                >=
```


<h3 sectionId="yo.lex.literals">Literals</h3>

<h4 sectionId="yo.lex.literals.int">Integer literals</h4>
An integer literal is a sequence of digits. Depending on the prefix, the literal is interpreted as base 2, 8, 10 or 16.

| Prefix | Base        |
| :----- | :---------- |
| `0b`   | binary      |
| `0o`   | octal       |
| `0x`   | hexadecimal |
|  none  | decimal     |

```
binary_literal   =  0b[01]+       // base 2
octal_literal    =  0o[0-7]+      // base 8
decimal_literal  =  [0-9]+        // base 10
hex_literal      =  0x[0-9a-f]+   // base 16
```

<h4 sectionId="yo.lex.literals.float">Floating-point literals</h4>
TODO

<h4 sectionId="yo.lex.literals.char">Character literal</h4>
A character literal is a valid ascii codepoint, enclosed by single quotes.
TODO

<h4 sectionId="yo.lex.literals.string">String literals</h4>
A string literal is a sequence of valid ascii codepoints enclosed by double quotes.  
There are multiple kinds of string literals:

- **Regular string literals**  
  The text between the quotes is interpreted as a sequence of character literals

- **Raw string literals**  
  Prefixed by a `r`. The quoted text is taken "as is", with no special handling whatsoever

- **Byte string literals**  
  Prefixed by a `b`. They represent a pointer to a sequence of ascii bytes

The `b` and `r` prefixes can be combined to create a raw bytestring.

| Literal    | Characters         | Type      |
| :--------- | :----------------- | :-------- |
| `"a\nb"`   | `a`, `\n`, `b`     | `*String` |
| `r"a\nb"`  | `a`, `\`, `n`, `b` | `*String` |
| `b"a\nb"`  | `a`, `\n`, `b`     | `*i8`     |
| `br"a\nb"` | `a`, `\`, `n`, `b` | `*i8`     |




<h2 sectionId="yo.types">Types</h2>

<h3 sectionId="yo.types.primitive">Primitive types</h3>
Yo defines the following primitive types:

| Typename | Size (bytes) | Description           | Values                   |
| :------- | :----------- | :-------------------- | :----------------------- |
| `void`   | 0            | the void type         | n/a                      |
| `u{N}`   | N/8          | unsigned integer type | `0 ... 2^N-1`            |
| `i{N}`   | N/8          | signed integer type   | `-2^(N-1) ... 2^(N-1)-1` |
| `bool`   | 1            | the boolean type      | `true`, `false`          |
| `f64`    | 8            | IEEE-754 binary64     | todo                     |

- Valid integer type sizes are: N = 8, 16, 32, 64
- An integer type's prefix indicates its signedness: `i8` is a signed integer, `u8` an unsigned integer
- A pointer to a type is declared by prefixing the type with an asterisk: `*i8` is a pointer to an `i8`
- Pointers can only point to types with a size > 0. The yo equivalent of C's `void*` is `*i8`


<h3 sectionId="yo.types.fn">Functions</h3>
- Functions are declared using the `fn` keyword, have a return type and can take zero or more parameters
- Functions that omit the return type are assumed to return `void`

```rust
fn add(x: i64, y: i64) -> i64 {
    return x + y;
}
```

#### External function declarations
The `extern` annotation can be used to forward-declare a C function's signature. In forward declarations, the parameter names must be omitted and the function must not contain a body.

```rust
#[extern]
fn malloc(i64) -> *i8;
```

Extern functions can be declared variadic, by inserting `...` after the last fixed parameter:
```rust
#[extern]
fn printf(*i8, ...) -> i64;
```

#### Function pointer types
The following syntax denotes a function pointer type:
```rust
(A1, A2, ...) -> R
```
- `A1, A2, ...`: The function's parameter types
- `R`: The function's return type

**Example** A struct storing a function pointer
```rust
struct Foo {
    add: (i64, i64) -> i64
}
```



<h3 sectionId="yo.types.struct">Structs</h3>
Custom types can be defined using the `struct` keyword. All struct types are uniquely identified by their name. A struct type can have properties and a set of member functions (methods) associated with it. Member functions must be declared in a separate `impl` block.

- Instance methods are type member functions that can be called on an instance of the type. They must take `self: typename` as their first parameter
- Static methods are type member functions that can be called on the type itself
- The compiler auto-generates an initializer for every type (`type_name::init(type_properties)`).

**Example:** Declaring a struct with properties and member functions  
```rust
struct Person {
    name: String,
    age: i8
}

impl Person {
    // no `self` parameter -> static method
    fn me() -> *Person {
        return Person::init("Lukas", 20);
    }

    // `self` parameter -> instance method
    fn increaseAge(self: *Person) {
        self.age += 1;
    }
}
```









<h2 sectionId="yo.expr">Expressions</h2>

Every expression evaluates to a value of a specific type, which must be known at compile time.

<h3 sectionId="yo.expr.cast">Type conversions</h3>

- **`static_cast`**
```rust
#[intrinsic]
fn static_cast<R, T>(arg: T) -> R;
```
The `static_cast` intrinsic converts an expression of type `T` to a related type `R` if there is a known conversion from `T` to `R`.
The following conversions are built-in:
  - i8 > u8
  - f64 > int

- **`reinterpret_cast`**
```rust
#[intrinsic]
fn reinterpret_cast<R, T>(arg: T) -> R;
```
The `reinterpret_cast` intrinsic converts between any two types `T` and `R`, by reinterpreting the value's bit pattern. `T` and `R` are required to have the exact same bit width.


<h3 sectionId="yo.expr.operators">Operators</h3>

- Prefix (unary) operators:

    | Operator  | Description |
    | :-------- | :---------- |
    | `-`       | negation    |
    | `~`       | bitwise NOT |
    | `!`       | logical NOT |

    These prefix operators - if defined for a type `T` - have the signature `(T) -> T`.

- Infix (binary) operators:
    
    Infix operators are listed in decreasing order of precedence.

    | Operator | Description           | Signature        | Precedence         |
    | :------- | :-------------------- | :--------------- | :----------------- |
    | `<<`     | bitwise shift left    | `(T, T) -> T`    | Bitshift           |
    | `>>`     | bitwise shift right   | `(T, T) -> T`    | Bitshift           |
    | `*`      | multiplication        | `(T, T) -> T`    | Multiplication     |
    | `/`      | division              | `(T, T) -> T`    | Multiplication     |
    | `%`      | remainder             | `(T, T) -> T`    | Multiplication     |
    | `&`      | bitwise AND           | `(T, T) -> T`    | Multiplication     |
    | `+`      | addition              | `(T, T) -> T`    | Addition           |
    | `-`      | subtraction           | `(T, T) -> T`    | Addition           |
    | `|`      | bitwise OR            | `(T, T) -> T`    | Addition           |
    | `^`      | bitwise XOR           | `(T, T) -> T`    | Addition           |
    | `==`     | equal                 | `(T, T) -> bool` | Comparison         |
    | `!=`     | not equal             | `(T, T) -> bool` | Comparison         |
    | `<`      | less than             | `(T, T) -> bool` | Comparison         |
    | `<=`     | less than or equal    | `(T, T) -> bool` | Comparison         |
    | `>`      | greater than          | `(T, T) -> bool` | Comparison         |
    | `>=`     | greater than or equal | `(T, T) -> bool` | Comparison         |
    | `&&`     | logical AND           | `(T, T) -> bool` | LogicalConjunction |
    | `||`     | logical OR            | `(T, T) -> bool` | LogicalConjunction |
    | `|>`     | function pipeline     | n/a              | FunctionPipeline   |
    | `=`      | assignment            | n/a              | Assignment         |



<h4 sectionId="yo.expr.operators.overloading">Operator overloading</h4>

Operators can be overloaded:
```rust
fn operator + (lhs: bool, rhs: bool) -> void;
```


<h3 sectionId="yo.expr.lambda">Lambdas</h3>
A lambda expression constructs an anynomous function





<h2 sectionId="yo.attr">Attributes</h2>
Attributes can be used to provide the compiler with additional knowledge about a declaration.

<h3 sectionId="yo.attr.fn">Function Attributes</h3>

| Name    |      |
| :------ | :--- |
| `extern` | C linkage |
| `intrinsic` | (internal) declares a compile-time intrinsic |
| `no_mangle` | Don't mangle the function's name |
| `mangle={string}` | Override a function's mangled name |
| `side_effects(...)` | Specify a function's side effects |

<!-- | `arc` | (wip!) enable arc on a per-function basis | -->

<h3 sectionId="yo.attr.struct">Struct Attributes</h3>

| Name    |      |
| :------ | :--- |
| `no_init` | The compiler should not generate a default initializer for the type |

<!-- | `arc` | (wip!) enable arc on a per-struct basis | -->

**Note:**
- the `no_mangle`, `mangle={string}` and `extern` attributes are mutually exclusive
- the `no_mangle` attribute can only be applied to global function declarations

**Example:** forward-declaring a variadic C function

```rust
#[extern]
fn printf(*i8, ...) -> i64;

// All of the following calls are valid:
printf(b"\n");
printf(b"a: %i\n", 2);
printf(b"other string: %s\n", b"text");
```







<h2 sectionId="yo.template">Templates</h2>
Templates provide a way to declare a generic implementation of a struct or function.

Templates don't exist "on their own": No code is generated when you only declare, but never use a template.  
When the compiler encounters an instantiation of a struct template or a call to a template function, it generates a specialized version for the supplied generic arguments.

```rust
// A function template
fn add<T>(x: T, y: T) -> T {
    return x + y;
}
```

Function specializations can be declared simply by overloadding the function for a specific signature.




<h2 sectionId="yo.mem">Memory Management</h2>
Yo currently doesn't have garbage collection / automatic reference counting.

There are two functions of note here:
- **`fn alloc<T>(count: size_t) -> *T`**  
    Allocates memory for `count` objects of type `T` and returns a pointer to be first byte of the allocated memory block.  
    The allocated memory is initialized to zero.
- **`fn dealloc<T>(ptr: *T) -> void`**  
    Deallocates a memory block allocated by `alloc`.



<script src="{{ '/static/spec-sections.js' | relative_url }}">
