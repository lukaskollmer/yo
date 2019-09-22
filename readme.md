# Yo
> The Yo Programming Language

<br>

Yo is a compiled programming language with strong static typing.

## Example

```rust
#[extern] fn puts(*i8) -> i32;

fn main() -> i32 {
    puts(b"Hello World!");
    return 0;
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
