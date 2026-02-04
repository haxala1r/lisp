This document holds my design notes for lexical and global environments
for this compiler. I have not yet named the language.

# Closures

The environment system implements flat closures.
When a closure is created at runtime, all free variables
it uses are packaged as part of the function object, then the function
body uses a GetFree instruction to get those free variables by an index.

(Free variables are propagated from inner closures outwards. This is necessary,
as this also handles multiple-argument functions gracefully.)

```scheme
(let ((a 10))
  (print (+ a 5)))
```

This code will be compiled as a lambda that takes a single parameter and executes
the body `(print (+ a 5))`, which is called immediately with the value 10.

The compiler tries to perform symbol resolution on expressions in the body of the
let as well, however it sees no other expressions creating further scopes.

Since there are two free symbols in this code (`+` and `print`), and the surrounding
environment does not have these two symbols defined locally, both of these symbols
will be resolved to their global definitions directly.

Now let's examine a classic example of closures:

```scheme
(define (adder x)
  (lambda (y) (+ x y)))
```

The adder function takes an argument x, and creates returns a function that adds x
to its argument.

This is implemented by a compiler pass that resolves symbols. Starting from top-level
expressions, it scans downwards, noting every free symbol. A free symbol is one
that is used in an expression, yet has no value defined locally in that expression.
In other words, its value must come from the surrounding scope.

In this example, the adder function has a symbol x that is a part of its function definition.
This is clearly not a free variable. However, examining the inner lambda expression,
we can see that it uses y (which is not free) and x. The value of x is not defined
as part of the lambda expression, so it must be free.

The compiler, seeing this, notes that the inner lambda has a free variable `x`, and a parameter
`y`. Thus, the lambda has 1 free variable and 1 parameter. This means the closure object will have
a code pointer along with an array of length 1 forming the storage for the free variable(s).
The compiler compiles the body of the lambda such that every occurance of `x` is replaced
with code to get free variable #0 from the current closure. (`y` is, naturally, parameter #0).
Otherwise, no special handling is necessary.

The inner lambda has no other expressions creating further scopes, so the compiler
knows it has hit the deepest scope in the expression, and starts scanning outwards once again.

Scanning outwards, the compiler sees that there is a defined symbol x, and in the scope
of this definition, a lambda expression that uses a free symbol named x is used. The
compiler matches these, and compiles the lambda expression (as in, the value that the lambda
expression will evaluate to) such that it creates a closure object: a pair of code pointer
pointing to the already compiled body, and an array of length 1 containing the current
value of x.

This newly created value represents the closure. As you might notice, the current value
of x has been copied into the closure object. The closure is now returned, and the
scope of `adder` is destroyed. The closure object survives.

Note: in actuality, the outer `adder` function itself is also a closure. The inner
lambda actually has *two* free variables: `+` is also a symbol, and its value is not
defined in the body of the lambda. Since `adder` also doesn't define it, the free symbol
is propagated outwards, and adder also accesses it as a free variable. The compiler
(when propagating free symbols) eventually reaches the global environment, and
resolves these free symbols to their global definitions.

All global symbols are late-bound. Once the free symbol is propagated outwards to the global
definition, the compiler must notice this and insert an instruction to get the
value of a global symbol.

Thus, the following will raise an error at runtime:

```
(define (adder x)
  (lambda (y) (+ x y)))
(set! '+ 5)
; + now equals 5.
(adder 5 5)
```

Since `5` is not a function, it cannot be called, and this will raise an error.

## Note on boxing

Closure conversion makes some situations a bit tricky.

```
(let ((x 10))
  (let ((f (lambda () x))) ;; f captures x
    (set! x 20)            ;; we change local x
    (f)))                  ;; does this return 10 or 20?
```

In this case, instead of x being copied directly into the closure, a
reference to its value is copied into the closure. This is usual in
most schemes and lisps.

In fact, you can even treat these as mutable state:

```
(define (make-counter)
 (let ((count 0))
  (lambda ()
    (set! count (+ count 1))
    count)))
```

So a closure can capture not just the value of a symbol, but also a
reference to it. This reference survives the end of the `make-counter`
function.

## Note on currying

Because this language is actually a curried variant of lisp/scheme, the
above function could also be written like this:

```scheme
(define (adder x y) (+ x y))
```

or, even like this:

```scheme
(define adder +)
```

... since the built-in `+` function is also already curried. In fact, the entire
language is curried. All function calls are (or behave as if they were) unary.
The function call syntax `(f x y)` is actually treated as `((f x) y)` by the
compiler.

## Note on syntax

I am using more or less regular Scheme syntax in this document. However, this is
potentially subject to change. I have not decided on what the official syntax
should be like. I am using Scheme syntax simply because I think it is fairly clean,
but some changes might make sense in the future as the semantics of this language
deviate greatly from Scheme's.

## Note on performance

This design document may raise concerns of performance. If everything above is
truly set in stone, then it seems obvious that there should be a performance
penalty.

As written, this design requires a basic addition like `(+ 1 2)` to allocate a
closure object after all. No matter how fast OCaml's minor heap may be
(and it is plenty fast, to be fair), that is not going to go well in a tight loop. 

These are valid concerns, and I am currently leaving these problems to my future
self.

Optimizing multiple-argument functions is actually fairly straightforward (or
it looks easy, at least), however I want to first make sure the language
has consistent semantics. A slow language is better than no language, after all.
So I intend to add the facilities necessary for these optimizations into the
compiler at a later point.

## Global Definitions

Global definitions get a separate section because they're mostly straightforward.

Any symbol defined through a top-level `define` form is made globally available
after the definition form. More accurately, the symbol is present in the program
before the define is reached, however it will be bound to a dummy value until
it is accessed.

This behaviour is proposed for the purpose of allowing mutually
recursive definitions without issue, however please note that this is not yet certain,
because this design comes with the tradeoff that errors involving symbols accessed
before the point they are supposed to be defined can only be detected at runtime.

To illustrate the problems this could cause:

```
(define b (+ a 10))
(define a 5)
```

This is pretty clearly an error - yet the compiler cannot, as proposed, determine
this. In the future, further passes over the source code could be added to scan
for such issues, or a differentiator between top-level function and variable
definitions to prevent this.

Notably, this problem does not occur for function definitions. In fact, the following
is perfectly fine despite looking a bit similar:

```
(define (b) (+ a 10))
(define a 5)
```

Generally any symbol appearing in the body of a function, will only be compiled
to access that symbol. The symbol is only accessed once the function is called.
Thus, you can create mutually recursive functions at the top level with no issue.

The body of the definition is only executed once the `define` form is reached.
Thus, definitions with side effects will execute exactly in the order they
appear in the source.

