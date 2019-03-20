# yo
> the yo programming language

> **Note:** There's a partially working llvm-based version in the [`rewrite`](https://github.com/lukaskollmer/yo/tree/rewrite) branch

<br>

Yo is an interpreted, statically typed programming language with a rust-inspired syntax. This repo includes a parser, compiler and interpreter, as well as a small standard library.

## Example

```rust
use "runtime";
use "io";

fn main(): int {
    io::print("Hello World");
}
```


## Documentation
You can find some documentation [here](https://lukaskollmer.me/yo)


## Usage
Build requirements: Swift 4.2, make, xcodebuild, xcpretty

```bash
# Get the source code
$ git clone https://github.com/lukaskollmer/yo && cd yo

# Build
$ make build [CONFIG=Debug|Release]  # default config is Debug

# Run
$ ./build/Debug/yo [options] <input>  # pass --help for a list of all options
```



## License
MIT @ [Lukas Kollmer](https://lukaskollmer.me)
