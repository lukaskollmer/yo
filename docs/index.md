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

- <a class="casual-underlined" href="/reference.html">Language Reference</a>
<!-- - <a class="casual-underlined" href="/docs/index.html">Documentation</a> -->


## Usage

### Building Yo

```bash
$ git clone https://github.com/lukaskollmer/yo && cd yo
$ cmake . && make
```

Build requirements:
- make
- cmake 3.10+
- python 3.6+
- llvm 8
- a C++17 compatible compiler


### Command Line Options

```bash
OVERVIEW: The Yo Programming Language v0.0.1

USAGE: yo [options] <input file>

OPTIONS:

General Options:

  -O                  - Enable Optimizations
  -dump-llvm          - Dump LLVM IR
  -dump-llvm-pre-opt  - Dump LLVM IR prior to running optimizations
  -emit               - Output format(s)
    =asm              -   Assembly
    =llvm-ir          -   LLVM IR
    =bin              -   Binary
    =obj              -   Object File
    =none             -   None
  -g                  - Emit debug metadata
  -print-ast          - Print the Abstract Syntax Tree
  -pygmentize         - Lex input, then print pygmentized HTML to stdout
  -run                - Run the generated executable after codegen. Implies `--emit bin`
  -run-args=<string>  - Argv to be used when executing the produced binary. Implies `-run`
  -stdlib-root=<path> - Load stdlib modules from <path>, instead of using the bundled ones

Generic Options:

  -help               - Display available options (-help-hidden for more)
  -help-list          - Display list of available options (-help-list-hidden for more)
  -version            - Display the version of this program
```

#### Useful options inherited from LLVM
- `--x86-asm-syntax={intel|att}`  
    Use in conjunction with `--emit asm` to set the assembly style to be emitted by LLVM's X86 backend. Defaults to `att`.