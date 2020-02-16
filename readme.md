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
You can find some documentation [here](https://lukaskollmer.de/yo-lang/)


## Usage
Build requirements:
- Make
- CMake 3.10+
- LLVM 8
- Python 3.6+
- Clang or GCC
- GoogleTest (optional, required if building tests)


## License
MIT @ [Lukas Kollmer](https://lukaskollmer.me)
