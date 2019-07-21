---
title: Home
layout: default
---

Yo is a compiled programming language with strong static typing

```rust
#[extern] fn atoi(*i8): i32;

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
Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.