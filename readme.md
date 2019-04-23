# yo
> the yo programming language

> **Note:** There's a partially working llvm-based version in the [`rewrite`](https://github.com/lukaskollmer/yo/tree/rewrite) branch

<br>

Yo is a compiled programming language with strong static typing.

## Example

```rust
use "runtime";
use "io";

fn main(): i32 {
    io::print("Hello World");
}
```


## Documentation
You can find some documentation [here](https://yo.lukaskollmer.me)


## Usage
Build requirements:
- cmake, make
- llvm 7
- clang (gcc probably works as well)


## License
MIT @ [Lukas Kollmer](https://lukaskollmer.me)
