# Values

A value is one of:

- Integer
- Float
- String
- Symbol
- Nil
- Cons cell (a.k.a. pair)
- Boxed value (see [Immutability and Boxing](immutability.md))
- Closure

## Numeric values

The numeric types have the default size for their type on the target system (64 bits for integers, doubles for floats on modern 64-bit systems). There is currently no built-in bigint support.

Type predicates:

- `(number? v)`: equivalent to `(or (int? v) (float? v))`
- `(int? v)`: evaluates to `'t` if v is an integer, `()` otherwise.
- `(float? v)`: evaluates to `'t` if v is a float, `()` otherwise.

Mathematical operators "promote" their arguments from ints to floats whenever
appropriate - `floor`, `ceil` or `round` may be used to truncate a float
back to an int if this is undesirable.

For arithmetic operators (and similar multiple-argument operators) this
typically means the result will be "promoted" whenever at least one of
the arguments is a float (or in the case of `/`, always).

- `+`: addition
- `-`: subtraction
- `*`: multiplication
- `/`: division

Note that there are no variadic functions in olisp,
so `(+ 1 2 3)` is invalid. See [Differences From Typical Lisps](differences.md).

## Strings

Currently strings are handled directly in the runtime system. The language
has no vector support yet, but once vectors are added strings may be replaced
with a special vector type.

- `(string? v)`: type predicate.
- `(string=? v1 v2)`: equality predicate.
- `(string+ v1 v2)`: concatenates v1 and v2.


## Symbol

Symbols are special, atomic values. They may be created via quoting:

```lisp
(print (quote my-symbol))

; special syntax ' may be used for quoting:
(print 'my-symbol)
```

Symbol values are meant to be efficient placeholder values. They
may be used freely, however keep in mind that there are a few
differences between symbols in this language and typical lisps:

- there is no way to "intern" a symbol value at runtime
- the language in general is much less dynamic when it comes to
symbols. You may not access a symbol's "value slot" like you can
in Common Lisp, for example, given a runtime symbol value.

Symbols are meant to be efficient (in terms of comparison among values)
and unique. The current runtime keeps the entire runtime string around
at runtime, and while this is required to be able to *print* symbols,
it is a hindrance when performing frequent comparisons (a string comparison
is O(N) compared to expected O(1) for atomic values).

## Cons cells

A "cons cell" is simply a *pair*. However, these *pairs* are also used
as the basis for *linked lists*, hence why `list?` and `cons?` are equivalent
(except for the nil case, because nil is an empty list, but it is *not*
a cons cell).

A *list* refers specifically to a *linked list* formed of cons cells.

Lists are the fundemental data structure the language is built on.
Source code is represented directly in terms of linked lists
(and other primitive value types).

- `nil?`: `t` if nil, `nil` otherwise
- `list?`: equivalent to `(or (cons? v) (nil? v))`

Additionally, there are many functions specifically designed
to make it easier to manipulate and process lists:

- `(map f l)`: maps `f` over the list `l`, i.e. applies `f` to
every element of `l` and returns a list containing the results.
- `(foldl f init l)`: a left-fold over `l` with binary function `f`
- `(filter p l)`: filters l to only keep elements that satisfy
predicate `p`
- `(remove p l)`: filters l to remove elements satisfying
predicate `p`

Nil is a special placeholder value. It is the canonical false value,
as well as representing the empty list. It is a self-evaluating
symbol similar to `t`, but `()` also evaluates to nil.

## Other types

In addition, the *continuation* type may also be present in a conforming
system. The short story is: all user code is compiled into Continuation
Passing Style. This practically means two things: 

1. each user function actually takes an additional parameter `k`: the continuation.
2. there must be additional closure objects, not defined by the user, that facilitate
this continuation passing.

Thus, a conforming implementation may distinguish between continuation closures,
(which don't take a continuation parameter) and regular closures (which must
take a continuation parameter) for optimization purposes.

Regardless, user code must observe `closure?` evaluating to `'t` for any
closure objects, whether they are distinguished as such or not.
