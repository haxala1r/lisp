# Immutability and Boxing

A symbol `define`d at the top level may not be reassigned.
Local symbols may not be reassigned. The following are invalid:

```
(define v 15)
(set! v 16) ; invalid!

(define (func)
  (let ((x 1))
    (set! x 2))) ; invalid!
```

## Boxing

The canonical way to achieve mutation is to use a boxed value:

```
(define v (box 15))

(set-box! v 16)

(print (get-box! v))
```

This is valid. The important thing to recognize here is that the
value bound to `v` has not actually changed through this operation.
Instead, the value of `v` is a box (a pointer) to a value, and
we have changed. the value *stored* there.

The `set-box!` function is intended to be a runtime built-in.

## When Boxing is Not Required

While it is true that `box`ing is required for simple mutation,
it is not actually required to use explicit `box`es everywhere.

For objects that are already required to be allocated on the heap,
such as cons cells or user-defined record types (to be added later on),
`set-<attr>!` functions will automatically be defined to modify
attributes.

For cons cells specifically, `set-car!` and `set-cdr!` functions are
already defined as part of the runtime.

## Shadowing

Note that while local and global variables may not be *reassigned* they
absolutely can be *re-bound* or shadowed:

```
(let ((x 5))
  (let ((x 6))
    x)) ; => evaluates to 6
```

