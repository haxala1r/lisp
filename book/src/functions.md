# Functions

Functions play a central role in olisp - they are the primary means
of abstraction. 

## Named functions

You may define functions on the top-level through a `define` form:

```
; function that multiplies its argument by two
(define (double x) (* x 2))

; calculates the factorial of a number
(define (factorial n)
  (if (=? n 0)
    1
	(* n (factorial (- n 1)))))
```

Perhaps more formally, a `define` form constitutes a function
definition when the *symbol* portion of a `(define symbol value)`
form is syntactically a list. The first element of the list
shall be a symbol (denoting the symbol the function will be bound
to), and the rest of the list shall be the parameter list
of the function (as with a `lambda` form).

## Lambda

You may create unnamed functions through the use of `lambda`:
`(lambda (x) (+ x 1))` evaluates to a function incrementing its
sole argument by one.

Note that although they may seem different, the distinction
between named and unnamed functions is mostly in debugging
and error messages. Otherwise, a lambda form and a define
form are mostly equivalent. In fact, `(define (f ...) ...)`
is desugared into `(define f (lambda (...) ...))` until
it reaches the core AST (not exactly, since debug information
has to be attached, but they are practically equivalent).

A lambda form is a special construct. It is
syntactically similar to a function call, but it is not one.
`lambda` receives the rest of its application body completely
unevaluated.

The second element in the list constituting a `lambda` invocation
shall be the parameter list of the function to be created. This
parameter list shall be a list of symbols. Each symbol will
be (positionally) bound to the supplied argument when the 
function is invoked. 

The rest of the `lambda` invocation,
after the second element, is the body. The body is a
series of expressions, each of which will be evaluated
one by one in the order given in the body. The function
invocation evaluates to the value of the last
form in the body.

The body of a lambda may be empty. Such a function always
evaluates to nil.

## Function Values

A defined function may be invoked as such: `(factorial 5)`.

Although functions may be directly invoked like this, this is not
all that can be done with functions. Functions are values, just like any other.
They may be assigned to variables, kept in lists, passed to other functions...

Indeed, some standard library functions even *expect* functions
as their arguments: `map`, `foldl`, `filter` being some examples.
You may pass a lambda form directly to these functions: 
`(map (lambda (x) (+ x 1)) '(1 2 3 4))`

Another way would be to define your function separately, and simply
pass the function directly:

```
(define (inc x) (+ x 1))
(map inc '(1 2 3 4))
```


