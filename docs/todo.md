- [x] reference counting
- [ ] `self.foo += x;` assignments (currently kinda works as a side effect of implicit seld access, ie `foo += 1` if self has a foo attribute)
- [x] chained property access
- [x] chained method calls
- [ ] tests
- [x] bitwise operators (NOT, AND, OR, XOR)
- [x] bitshift operators
- [x] implement bools
- [ ] add support for floating point numbers (custom instructions `addf`, `subf`, `mulf` `divf`, what about `mod`, `not`, etc?)
- [x] array literals (TODO improve)
- [x] nested binops (`((1 << 4) << 7)`) (TODO check, this might already work)
- [x] binary number literals
- [x] `ret obj[0] & 1048575;`
- [x] underscores in number literals
- [ ] implicitly import stdlib modules (ie import "std/string" if the code contains a string literal, same for numbers, arrays)
- [ ] test that the examples in the documentation actually work
- [x] auto-synthesize `dealloc` functions (only if not otherwise defined)
- [ ] throw an error when the stack grows into allocated heap space / vice versa
- [x] implement some sort of `fatalError` function to abort w/ an error message
- [x] `Array.get`: negative index to access elements from back
- [ ] ternary expressions (only for assignment at first)
- [ ] elvis operator (`a ?? b`)
- [x] `else if`
- [x] add type checks when assigning the return value of a function?
- [ ] macros
- [ ] make `nil` a macro for `0 as id`
- [x] `break` out of `for`/`while` loops
- [ ] ensure that all custom `dealloc` methods return `void`
- [ ] ensure that `void` returning functions don't return anything other than `void`
- [ ] disallow `void` variable declarations (and function parameters)? they're currently treated as ints (as in also getting allocated stack space)
- [ ] support casts in chained access `(5 as Foo).bar`
- [x] `Self` to refer to whatever the current type is
- [ ] short circuit evaluation
- [ ] compile time check to make sure that format functions are passed the correct number of arguments (maybe introduce some sort of annotation or macro that can be applied to functions to tell the compiler to run that check, like objc's `NS_FORMAT_FUNCTION`)
- [ ] remove unused functions from the generated instructions?
- [ ] custom string literal prefix/suffix functions (like what ES6 and c++ have) (maybe via a macro thing)
- [ ] pass static function reference as argument (ie `array.forEach(io::printi)`)
- [ ] allow `val` etc to be used as identifiers, if fit
- [ ] make sure typenames used in function signatures actually exist
- [ ] `#expr` to get an array's length
- [ ] reset parsed annotations when they'd become invalid (not sure if this is actually a problem)
- [ ] allow disabling arc for an entire type/impl block
- [ ] allow disabling getters/setters for individual attributes
- [ ] don't evaluate function arguments right to left
- [x] simple heap array literal. ie `val x = {1, 3, 4};` would call `runtime::alloc(3)` and fill that w/ 1, 3 and 4
- [ ] always zero out the newly allocated backing range on `runtime::alloc`
- [ ] throw descriptive error when accessing a nonexistent attribute
- [x] allow `ret;` instead of `ret 0;` in void functions
- [ ] if a function doesn't specify a return type, use `void` as default
- [ ] type inference for static variables
- [ ] make `namespace`s a thing
      (collection of static functions, basically the same that can already be achieved by using an `impl` for a nonexistent type)
      The difference would be that a) you can't declare a `type` or `struct` w/ that name and b) all functions are implicitly static
- [x] `defer`
- [ ] don't allow creating attributes w/ nonexistent types (seems to work fine so far which it shouldnt)
- [ ] comment at end of file crashes lexer?
- [ ] can't call native function in lambda?
- [x] operator precedence
- [ ] top level `defer` statements?
- [ ] move `defer` blocks to after the temp retval assignment, but before the `ret` instruction?
- [ ] redeclare primitive types as structs? that'd allow int/etc to have member functions
- [ ] `use` paths case insensitive? (currently they are not and `use "std/Array";` will cause many issues)
- [ ]  `io::print(a.get(3).description());` chained method calls
- [ ] builtin functions that are just a single instruction and therefore can be inlined?

**lambdas**
- [ ] what if a lambda declares `x`, but there's alao an `x` in the outer scope? (it should obviously NOT capture the outer `x`)
- [ ] can't call captured lambda in lambda w/out having to assign it to a local variable first?
- [ ] allow overriding a lambda's return type? (TODO is there a use case for this?)
- [ ] allow lambdas w/out the parens and a single expression?

**conditions**
- [ ] OR comp where both sides are true fail (ie `true == true || false == false`)

**native function naming**
- [ ] use underscore to indicate that a function is implemented as a native function and therefore doesn't retain/release its arguments?

**variadic functions**
 - [ ] if a check that all non-fixed arguments to a variadic function expecting `Array` are complex
 - [ ] add a `...` spread operator for passing an array to a variadic function
       basically, what this should do is pass the array as is, w/out creating a new array containing the non-fixed elements
       would only work if there is only 1 non-fixed element, which is an array (primitive or complex)
