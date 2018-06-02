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

- You can declare a function using the `fn` keyword
- Every function is required to specify a return type
- If the function body doesn't end with a return statement, it implicitly returns 0

```rust
// Declare a function that takes 0 parameters and returns an integer
fn main(): int {
    ret 12;
}
```



## License
MIT @ [Lukas Kollmer](https://lukaskollmer.me)
