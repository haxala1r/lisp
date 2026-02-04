This document holds my design notes for lexical and global environments
for this compiler. I have not yet named the language.

# Environments

An environment is an integral part of the runtime of the language.
There is a global environment that holds the values of all global
symbols.

Lexical environments generally don't exist in practice, instead we use
flat closures. When a closure is created at runtime, all free variables
it uses are packaged as part of the function object, then the function
body uses a GetFree instruction to get those free variables by an index.

Free variables are propagated from inner closures outwards. This is necessary,
as this also handles multiple-argument functions gracefully.


```scheme
(let ((a 10))
  (print (+ a 5)))
```


## Global Definitions

Any symbol defined through a top-level `define` form is made globally available
after the definition form.

This is the most common use for define.

Generally any symbol appearing in the body of a function, will only be compiled
to access that symbol. The symbol is only accessed once the function is called.
Thus, you can create mutually recursive functions at the top level with no issue.

## Local Definitions

It is valid to use `define` forms in body sections. Informally, a body section
is the body of most built-in forms, including `lambda`, `let`, and `letrec`.


