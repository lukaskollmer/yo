---
title: Home
layout: default
---

Yo is a compiled programming language with strong static typing

```rust
#[extern] fn puts(*i8) -> i32;

fn main() -> i32 {
    puts(b"Hello World!");
    return 0;
}
```

## Learn more

- <a class="casual-underlined" href="/spec.html">Language Specification</a>
- <a class="casual-underlined" href="/docs/index.html">Documentation</a>


## Usage

Build requirements:
- make
- cmake 3.10+
- python 3.6+
- llvm 8
- clang (gcc probably works as well)

```bash
$ git clone https://github.com/lukaskollmer/yo && cd yo
$ cmake . && make
$ ./yo -help
OVERVIEW: the yo programming language v0.0.1

USAGE: yo [options] <input file> <run args>...

OPTIONS:

General Options:

  -O                  - Enable Optimizations
  -arc                - [internal] enable arc
  -dump-llvm          - Dump LLVM IR
  -dump-llvm-pre-opt  - Dump LLVM IR prior to running optimizations
  -emit-llvm          - Emit LLVM IR
  -print-ast          - Print the Abstract Syntax Tree
  -run                - Run the generated executable after codegen
  -stdlib-root=<path> - Load stdlib modules from <path>, instead of using the bundled ones

Generic Options:

  -help               - Display available options (-help-hidden for more)
  -help-list          - Display list of available options (-help-list-hidden for more)
  -version            - Display the version of this program
```
