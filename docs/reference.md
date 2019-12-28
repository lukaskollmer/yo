---
layout: default
title: Reference
---




> **Note** This is very much still work in progress and does not necessarily describe the language as implemented in the [GitHub repo](https://github.com/lukaskollmer/yo)


<div id="toc"></div>


<!--
TODO:
- only have two levels in the main toc, then add a small local toc to the beginning of each section?
-->








<!--
YO.LEX
-->



<h2 sectionId="yo.lex">Lexical structure</h2>
Yo source code is written in ASCII. Some UTF-8 codepoints will probably work in identifiers and string literals, but there's no proper handling for characters outside the ASCII character set.


<h3 sectionId="yo.lex.comment">Comments</h3>
There are two kinds of comments:
- Line Comments, starting with `//` and continuing until the next line break (`\n`)
- Comment Blocks, starting with `/*` and continuing until the next `*/`. Comment blocks cannot be nested


<h3 sectionId="yo.lex.tokens">Tokens</h3>
The Yo lexer differentiates between the following kinds of tokens: keywords, identifiers, punctuation and literals.


<h3 sectionId="yo.lex.keyword">Keywords</h3>
Yo reserves the following keywords:

```
break       else    impl    match       switch    while
continue    fn      in      operator    unless
decltype    for     let     return      use
defer       if      mut     struct      var
```


<h3 sectionId="yo.lex.ident">Identifiers</h3>
An identifier is a sequence of one or more letters or digits. The first element must not be a digit.
```
digit  = [0-9]
letter = [a-zA-Z_]
ident  = <letter>(<letter>|<digit>)*
```

A sequence of characters that satisfies the `ident` pattern above and is not a reserved keyword is assumed to be an identifier.  
All identifiers with two leading underscores are reserved and should be considered internal.


<h3 sectionId="yo.lex.operators">Operators and punctuation</h3>

The following character sequences represent [operators](#yo.expr.operators) and punctuation:
```
+    &    &&    ==    |>    (    )
-    |    ||    !=    =     {    }
*    ^          <     !     [    ]
/    <<         <=          .    ;
%    >>         >           ,    :
                >=
```


<h3 sectionId="yo.lex.literals">Literals</h3>

<h4 sectionId="yo.lex.literals.numeric">Numeric literals</h4>
An integer literal is a sequence of digits. Depending on the prefix, the literal is interpreted as base 2, 8, 10 or 16.

| Prefix | Base        |
| :----- | :---------- |
| `0b`   | binary      |
| `0o`   | octal       |
| `0x`   | hexadecimal |
|  none  | decimal     |

A base-10 integer literal which is followed by a peroid (`.`) is interpreted as a floating point literal.

```
binary_literal   =  0b[01]+       // base 2
octal_literal    =  0o[0-7]+      // base 8
decimal_literal  =  [0-9]+        // base 10
hex_literal      =  0x[0-9a-f]+   // base 16

flt_literal      = <decimal_literal>.<decimal_literal>
```


<h4 sectionId="yo.lex.literals.char">Character literal</h4>
A character literal is a valid ascii codepoint, enclosed by single quotes.


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













<!--
YO.MODULE
-->



<h2 sectionId="yo.module">Modules</h2>

Yo source code is organized in modules.
Every `.yo` file is considered a module, uniquely identified by its absolute path.

The `use` keyword, followed by a string literal, imports a module:

```rust
use "<path>";
```

Note that this is essentially the same as C++'s `#include` directive, as in that the parser will simply insert the contents of the imported module at the location of the `use` statement.
If a module has already been imported, future imports of the same module will have no effect.


<h3 sectionId="yo.module.resolve">Path resolution</h3>
<!-- **Path resolution**   -->
Import paths are resolved relative to the directory of the module containing the `use` statement.

<!-- Consider the following directory structure:
```
src/
    A/
        foo.yo
        bar.yo
    B/
        foo.yo
    main.yo
    foo.yo
```

use foo in main will resolve to src/foo.yo, while [...]
-->


<h3 sectionId="yo.module.builtin">Builtin modules</h3>
<!-- **Builtin modules**   -->
The [`/stdlib`](https://github.com/lukaskollmer/yo/blob/master/stdlib/) folder contains several builtin modules.
These can be imported by prefixing the import name with a colon.

Builtin modules are bundled with the compiler, meaning that the actual stdlib/ files need not be present.  
However, the `-stdlib-root` flag can be used to specify the base directory all imports with a path prefixed by `:` will be resolved to.

**Example** Importing a builtin module
```rust
use ":runtime/core";
```













<!--
YO.TYPES
-->



<h2 sectionId="yo.types">Types</h2>

<h3 sectionId="yo.types.primitive">Primitive types</h3>
Yo defines the following primitive types:

| Typename | Size (bytes) | Description           | Values                                |
| :------- | :----------- | :-------------------- | :------------------------------------ |
| `void`   | 0            | the void type         | n/a                                   |
| `u{N}`   | N/8          | unsigned integer type | `0 ... 2^N-1`                         |
| `i{N}`   | N/8          | signed integer type   | `-2^(N-1) ... 2^(N-1)-1`              |
| `bool`   | 1            | the boolean type      | `true`, `false`                       |
| `f32`    | 4            | IEEE-754 binary32     | [see wikipedia][ieee754binary32_wiki] |
| `f64`    | 8            | IEEE-754 binary64     | [see wikipedia][ieee754binary64_wiki] |

[ieee754binary32_wiki]: https://en.wikipedia.org/wiki/Single-precision_floating-point_format
[ieee754binary64_wiki]: https://en.wikipedia.org/wiki/Double-precision_floating-point_format



<h3 sectionId="yo.types.int">Integer types</h3>

For integer types `u{N}` and `i{N}`, valid sizes are: N = 8, 16, 32, 64.  
An integer type's signedness is indicated by its prefix: `i8` is a signed integer, `u8` an unsigned integer.




<h3 sectionId="yo.types.float">Floating-point types</h3>

There are two types for floating-point values: `f32` and `f64`.  
The `f32` type represents a 32-bit wide IEEE-754 floating point value, the `f64` type a 64-bit wide IEEE-754 floating point value.




<h3 sectionId="yo.types.ptr">Pointer types</h3>

A pointer to a value of type `T` is expressed by prefixing the type with an asterisk (`*`).

For example, the type `*i32` denotes the set of all pointers to `i32` values.  
A pointer type's base type must be of size > 0.
Yo's equivalent of a C `void *` is `*i8`.




<h3 sectionId="yo.types.ref">Reference types</h3>

A [reference](#yo.ref) to a base type `T` is expressed by prefixing the type with an ampersand (`&`).  

```rust
let a: i64 = 12;
let b
```

See the [lvalue references](#yo.ref) section for more info.




<h3 sectionId="yo.types.fn">Function types</h3>

A function type represents all functions with the same parameter and result types:
```rust
        () -> void  // a function that has no parameters and returns nothing
(i32, i32) -> i64   // a function that takes two `i32` values and returns an `i64` value
```
A function type (ie, a [function](#yo.decl.fn)'s signature) only contains the types of the parameter and return types, it does not contain the names of the individual parameters or any [attributes](#yo.attr.fn) the actual function declaration might have.




<h3 sectionId="yo.types.decltype">decltype</h3>

```
decltype(<expr>)
```

The `decltype` construct can be used whenever the compiler would expect a type.
It takes a single argument - an expression - and yields the type that expression would evaluate to. The expression is not evaluated.

`decltype` is useful in situations where it would otherwise be difficult or impossible to declare a type, for example when dealing with types that depend on template parameters.

**Example**
```rust
fn add<T, U>(x: T, y: U) -> decltype(x + y) {
    return x + y;
}
```




<h3 sectionId="yo.types.typealias">Typealias</h3>

```rust
use A = B;
```

The `use` keyword, followed by an identifier, introduces a typealias.











<!--
YO.DECL
-->


<!-- yo.decl.fn -->

<h2 sectionId="yo.fn">Functions</h2>

<h3 sectionId="yo.fn.decl">Function declaration</h3>

A function is declared using the `fn` keyword. A function declaration consists of:
- *(optional)* the function's [attributes](#yo.attr.fn)
- the function's name
- *(optional)* the function's template parameters
- the function's parameters
- *(optional)* the function's return type
- the function's body

A function's return type may be omitted, in which case it defaults to `void`.

**Example**
```rust
// A simple function declaration
fn add(x: i64, y: i64) -> i64 {
    return x + y;
}
```



<h3 sectionId="yo.fn.decl.tmpl">Function template</h3>

In the case of a [function template](#yo.tmpl.fn) declaration, the template parameter names are listed in angled brackets, immediately prior to the function's parameter list.

**Example**
```rust
// The identity function
fn id<T>(arg: T) -> T {
    return arg;
}

// The add function from above, as a function template
fn add<T>(x: T, y: T) -> T {
    return x + y;
}
```

See the [templates](#yo.tmpl) section for more info.




<h3 sectionId="yo.fn.operator">Operator declaration</h3>

Since most [operators](#yo.expr.operators) are implemented as functions, they can be overloaded for a specific signature.
An operator overload is declared as a function with the name `operator`, followed by the operator being overloaded.

The following operators may be overloaded:
```
+    &     &&    ==    ()
-    |     ||    !=    []
*    ^           <
/    <<          >
%    >>          <=
                 >=
```

**Example** Overloading the addition operator for a custom type:
```rust
fn operator + (x: Foo, y: Foo) -> Foo {
    // some custom addition logic
}
```


**Overloading the call and subscript operators**  
A [struct type](#yo.struct) may overload the call and subscript operators.
These overloads must be defined in one of the type's `impl` blocks, and match the signature requirements for instance methods.

The `()` operator may accept an arbitrary number of parameters.
The `[]` operator must always accept exactly one parameter.

```rust
// Example: overloading the subscript operator
impl String {
    fn operator [] (self: &String, index: i64) -> &i8 {
        return self.data[index];
    }
}
```

**Note** When overloading comparison operators, implementing just `==` and `<` is sufficient, since all other operators have default implementations defined in terms of these two.




<h3 sectionId="yo.fn.resolution">Overload resolution</h3>

When generating code for a function call, the compiler will collect a set of potential target for the call.
From that set, the overload most closely matching the supplied arguments will be selected, based on a scoring system.
A tie (ie, two or more equally likely targets) will result in a compile-time error.










<!-- yo.struct -->

<h2 sectionId="yo.struct">Structs</h2>

<h3 sectionId="yo.struct.decl">Struct declaration</h3>

Custom types can be defined using the `struct` keyword. All struct types are uniquely identified by their name. A struct type can have properties and a set of member functions (methods) associated with it. Member functions are declared in one or multiple `impl` blocks.

- Instance methods are type member functions that can be called on an instance of the type. They must take `self` as their first parameter
- Static methods are type member functions that can be called on the type itself, using the `::` syntax
- Unless the `no_init` [attribute](#yo.attr.struct) is present, the compiler will synthesize an initializer for a struct type


**Example** Declaring a struct with properties and member functions  
```rust
struct Person {
    name: String,
    age: i8
}

impl Person {
    // no `self` parameter -> static method
    fn me() -> Person {
        return Person("Lukas", 20);
    }

    // `self` parameter -> instance method
    fn increaseAge(self: &Self) {
        self.age += 1;
    }
}
```


<h3 sectionId="yo.struct.static_method">Static methods</h3>

A static method is a function which can be called on the type itself.
All function declarations in an `impl` block that are not instance methods are static methods.

**Example**
```rust
struct Foo {}

impl Foo {
    fn bar() -> i64 {
        return 123;
    }
}
```



<h3 sectionId="yo.struct.instance_method">Instance methods</h3>

An instance method is a function defined in a type's `impl` block which a reference to the type as its first parameter.

```rust
struct Number {
    value: i32
}

impl Number {
    fn increment(self: &Self) {
        self.value += 1;
    }

    fn getValue(self: &Self) -> i32 {
        return self.value;
    }
}

let number = Number(10);
number.increment();
number.increment();
number.getValue();  // <- returns 12
```




<h3 sectionId="yo.struct.init">Struct initialization</h3>

Unless explicitly disabled via the `no_init` [attribute](#yo.attr), the compiler synthesizes the following initialization functions for a struct type:
- A constructor, which can be called to construct and initialize an instance of the type
- A default memberwise initializer, which sets each property of the object
- A default copy initializer, which initializes an object by copying another object's properties

An initializer is an instance method named `init` which returns `void`. A type can define custom initializers simply by overloading `init` for different signatures.

**Constructor**  
A type's constructor is invoked simply by calling the type as if it were a function:
```rust
let array = Array<Int>();
```
Based on the arguments passed to the constructor, the compiler will forward the call to one of the type's initializers.

**Memberwise initializer**  
The synthesized memberwise initializer takes the same arguments as the type's member fields, and simply sets the respective values. Note that if a type's member is a [reference](#yo.ref), the ccmpiler-generated memberwise initializer is the only option to initialize this reference (assigning to a reference member in a non-default initializer will set the object being referenced, as opposed to the reference itself).


**Copy initializer**  
The copy initializer takes two references to the current type (self and another object). It is used to initialize a an object from another instance of the same type, for example for constructing a copy when passing an object by-value to a function.




<h3 sectionId="yo.struct.deinit">Struct destruction</h3>

A type may implement a `dealloc` instance method, which will be invoked by the compiler when destructing that instance.
This method must take just the `self` parameter and return `void`.

**Note** custom `dealloc` methods will only be called for types that don't specify the `no_init` attribute


<!--
YO.EXPR
-->



<h2 sectionId="yo.expr">Expressions</h2>

Every expression evaluates to a value of a specific type, which must be known at compile time.


<h3 sectionId="yo.expr.literals">Literals</h3>

| Literal                     | Type      | Example   |
| :-------------------------- | :-------- | :-------: |
| Integer literal             | `i64`     | `12`      |
| Floating point literal      | `f64`     | `12.0`    |
| Character literal           | `i8`      | `'a'`     |
| String literal              | `*String` | `"text"`  |
| String literal (bytestring) | `*i8`     | `b"text"` |



<!-- yo.expr.operators -->

<h3 sectionId="yo.expr.operators">Operators</h3>

- Prefix (unary) operators:

    | Operator | Description |
    | :-------:| :---------- |
    | `-`      | negation    |
    | `~`      | bitwise NOT |
    | `!`      | logical NOT |

    These prefix operators – if defined for a type `T` – have the signature `(T) -> T`.

- Infix (binary) operators (in decreasing order of precedence):

    | Operator | Description           | Signature        | Precedence         |
    | :------: | :-------------------- | :--------------- | :----------------- |
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

**Note** Since most of the binary operators above are implemented as functions, they can be overloaded (see [yo.decl.fn.operator](#yo.decl.fn.operator))




<!-- yo.expr.cast -->

<h3 sectionId="yo.expr.cast">Type conversions</h3>

All type conversions are required to be explicit: Attempting to pass an `i64` to a function that expects an `u64` will result in a compilation error.

**Implicit conversions**  
The sole exception to this rule is numeric literals:
Even though numeric literals by default evaluate to values of type `i64`, you may use a literal in an expression that expects a different numeric type, and the compiler will implicitly cast the literal.


**Explicit conversions**  
There are two intrinsics for converting a value from one type to another:
- ```rust
  fn cast<To, From>(x: From) -> To;
  ``` 
  The `cast` intrinsic converts a value of type `A` to a related type `B`, if there exists a known conversion from `A` to `B`. If there is no such conversion, the cast will fail to compile.
- ```rust
  fn bitcast<To, From>(x: From) -> To;
  ``` 
  The `bitcast` intrinsic converts between any two types `A` and `B`, by reinterpreting the value's bit pattern. `T` and `R` must have the exact same bit width, otherwise the cast will fail to compile.


**Example**  
```rust
fn foo() -> i32 {
    let x = 0; // x has the deduced type i64
    return x;  // this will fail since the function is expected to return an i64
}

fn bar() -> i32 {
    return 0; // this will work fine since the compiler is allowed to insert an implicit static_cast<i32>
}
```




<!-- yo.expr.lambda  -->

<h3 sectionId="yo.expr.lambda">Lambdas</h3>

A lambda expression constructs an anynomous function.

Like a "normal" function, a lambda has a fixed set of inputs and an output.  
In addition, a lambda can also capture variables from outside its own scope (these captures must be explicitly declared in the lambda's capture list).  
<!-- A lambdas can define a [template](#yo.tmpl) parameter list. -->

There is no uniform type for lambda objects, instead the compiler will generate an anonymous type for each lambda expression.

**Syntax**
```
lambda_expr        = capture_list [tmpl_params] signature fn_body
capture_list       = "[" "]" | "[" capture_list_elem { "," capture_list_elem } "]"
capture_list_elem  = ["&"] ident [ "=" expr ]
```

**Example**
```rust
// a noop lambda: no input, no output, does nothing
let f1 = []() {};

// a lambda which adds two integers
let f2 = [](x: i64, y: i64) -> i64 {
    return x + y;
};

// a lambda which adds two values of the same type
let f3 = []<T>(x: T, y: T) -> T {
    return x + y;
};

// a lambda which captures an object by reference, and increments it
let x = 0;
let f4 = [&x](inc: i64) {
    x += inc;
};
```





<!--
YO.ATTR
-->



<h2 sectionId="yo.attr">Attributes</h2>
Attributes can be used to provide the compiler with additional knowledge about a declaration.

**Syntax**
```
attr_list = "#[" attr { "," attr } "]"
attr      = ident [ "=" attr_val ]
attr_val  = ident | string
```

A declaration that can have attributes can be preceded by one or multiple attribute lists.
Splitting multiple attributes up into multiple separate attribute lists is semantically equivalent to putting them all in a single list.

**Note** Specifying the same attribute multiple times with different values is considered undefined behaviour.

<h3 sectionId="yo.attr.types">Attribute Types</h3>

- `bool` The default attribute type. The value is determined simply by the presence of the attribute, unless explicitly stated.  
    Of the attribute lists below, `A` and `B` are equivalent, as are `C` and `D`:
    ```rust
    A  #[attr_name]
    B  #[attr_name=true]

    C  #[]
    D  #[attr_name=false]
    ```
- `string` In this case, the value must always be explicitly specified  
    ```rust
    #[attr_name="attr_value"]
    ```

<!-- TODO <h3 sectionId="yo.attr.syntax">Attribute Syntax</h3> -->

<h3 sectionId="yo.attr.fn">Function Attributes</h3>

| Name            | Type     | Description                                                     |
| :-------------- | :------- | :-------------------------------------------------------------- |
| `extern`        | `bool`   | C linkage                                                       |
| `inline`        | `bool`   | Function may be inlined                                         |
| `always_inline` | `bool`   | Function should always be inlined                               |
| `intrinsic`     | `bool`   | (internal) declares a compile-time intrinsic                    |
| `no_mangle`     | `bool`   | Don't mangle the function's name                                |
| `mangle`        | `string` | Override a function's mangled name                              |
| `startup`       | `bool`   | Causes the function to be called before execution enters `main` |
| `shutdown`      | `bool`   | Causes the function to be called after `main` returns           |

<!-- | `side_effects(...)` | Specify a function's side effects | -->
<!-- | `arc` | (wip!) enable arc on a per-function basis | -->

**Note**
- the `no_mangle`, `mangle={string}` and `extern` attributes are mutually exclusive
- the `no_mangle` attribute can only be applied to global function declarations


**Example**
```rust
// Forward-declaring a function with external C linkage.
#[extern]
fn strcmp(*i8, *i8) -> i32;

// A function with an explicitly set mangled name
#[mangle="bar"]
fn foo() -> void { ... }
```

<h3 sectionId="yo.attr.struct">Struct Attributes</h3>

| Name      | Type   | Description                                                         |
| :-------- | :----- | :------------------------------------------------------------------ |
| `no_init` | `bool` | The compiler should not generate a default initializer for the type |







<!--
YO.INTRINSIC
-->


<h2 sectionId="yo.intrinsic">Intrinsics</h2>

A function declared with the `intrinsic` [attribute](#yo.attr.fn) is considered a compile-time intrinsic. Calls to intrinsic functions will receive special handling by the compiler.
All intrinsic functions are declared in the [`:runtime/intrinsics`](https://github.com/lukaskollmer/yo/blob/master/stdlib/runtime/intrinsics.yo) module.

An intrinsic function may be overloaded with a custom implementation, in this case the overload must not declare the `intrinsic` attribute.









<!--
YO.REF
-->

<h2 sectionId="yo.ref">LValue references</h2>

_todo_




<!--
YO.TMPL
-->



<h2 sectionId="yo.tmpl">Templates</h2>
Templates provide a way to declare a generic implementation of a struct or function.

**Syntax**
```
tmpl_params = "<" tmpl_param { "," tmpl_param } ">"
tmpl_param  = ident [ "=" type ]
```

**Template codegen**

Templates don't exist "on their own": No code is generated when you only declare, but never use a template.  
When the compiler encounters an instantiation of a struct template or a call to a function template, it generates a specialized version for the supplied generic arguments.

```rust
// A function template
fn add<T>(x: T, y: T) -> T {
    return x + y;
}
```

Function specializations can be declared simply by overloadding the function for a specific signature.





<!--
YO.MEM
-->



<h2 sectionId="yo.mem">Memory Management</h2>

Yo implements C++-style RAII.
Types can define a custom [copy initializer](#yo.struct.init), which will be invoked when constructing a copy of an object, and a `dealloc` method, which will be invoked when an object goes out of scope.

<!-- The [`:runtime/memory`](https://github.com/lukaskollmer/yo/blob/master/stdlib/runtime/memory.yo) module declares some functions and intrinsics related to memory management:
- **`fn sizeof<T>() -> size_t`**  
    Returns the size of the template parameter type `T`, in bytes.
- **`fn alloc<T>(count: size_t) -> *T`**  
    Allocates memory for `count` objects of type `T` and returns a pointer to be first byte of the allocated memory block.  
    The allocated memory is initialized to zero.
- **`fn dealloc<T>(ptr: *T) -> void`**  
    Deallocates a memory block allocated by `alloc`. -->


<script src="{{ '/static/spec-sections.js' | relative_url }}"></script>

<script>
document.querySelectorAll('.markdown-body a:not([class])').forEach(n => {
    n.classList.add('casual-underlined');
});
</script>