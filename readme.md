# Yo
> The Yo Programming Language

<br>

Yo is a compiled programming language with strong static typing.

## Example

```rust
#[extern]
fn atoi(*i8): i32;

fn fib<T>(n: T): T {
    return match n {
        0, 1 => n,
        _ => fib(n-1) + fib(n-2)
    };
}

fn main(argc: i32, argv: **i8): i32 {
    return argv[1] |> atoi |> fib;
}
```


## Documentation
You can find some documentation [here](https://yo-lang.net)


## Usage
Build requirements:
- make
- cmake 3.10+
- llvm 8
- python 3.6+
- clang (gcc probably works as well)


## License
MIT @ [Lukas Kollmer](https://lukaskollmer.me)
