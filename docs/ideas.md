# yo

**all of this is outdated**

```rust
fn abs(x) {
  if x < 0 {
    ret -x;
  } else {
    ret x;
  }
}

fn main() {
  val x = abs(5);
  io::print("%s", x); -> '5'
  ret 0;
}
```


## types
```rust
type Name(first, last);

fn main() {
  val me = Name::new("lukas", "kollmer");
}
```

- `typename::new(...)` takes n parameters (n == # fields in the type) and initializes the values
- `typename::new()` sets everything to null

```rust
impl Name {
  fn greet() { // implicit self?
    io::print("hello, %s", self.first);
  }
}

// call like this:
val me = Name::new("lukas", "kollmer");
me.greet();
```

## importing other modules
```rust
import "io";
val io = import("io");
```

import other modules using the `import` function.
the return value of the `import` function is automatically assigned to a variable.

here's a rough pseudo-code description of `import`:
```rust
fn import(name) {
  // if there's a builtin module `name`, return that
  if runtime::builtins.contains(name) {
    ret runtime::builtins.get(name); // ie `import("io")
  }

  // if there is no builtin module `name`, we search the current directory
  //TODO

}
```


## varargs
if the last parameter is prefixed with `...`, it becomes an array

example:
```rust
fn format(fmt, ...args);

format("", 1, 2, 3); // args is [1, 2, 3]
```

## string literals?
introduce a new instruction that takes splits its immediate into 8 bit chunks and fills an array with them?

- strings are a type, which is backed by a primitive integer array on the heap
- the backing is 0-terminated
- these arrays should have a special retain count, indicating that they're immutable
- string literals in the source code are represented as a `runtime::alloc` call, followed by code that fills the array // TODO somehow bake them into the instructions?

- introduce an additional WIPinstruction type that



## protocols
```rust
type X();

protocol Y {
    fn bar(self: Foo): int;
}

impl X: Y {
    fn bar(self: Foo): int {
        // code
    }
}
```
