## Equality

The generic `eq?` comparison function compares *identities*
whenever possible. This means, for example, `(eq? (cons 1 ()) (cons 1 ()))`
would evaluate to `()`, despite the fact that the cons
cells being compared have the same contents - this is
because the two cons cells have different *identities*,
they are distinct objects on the heap.

This notion of equality may appear inconvenient, but it
is intended - this way, `eq?` calls can be reduced
to a direct pointer/value comparison at runtime. Whenever
different behaviour is required, it is recommended to
use a more specific comparison function, ideally one
specialised to your purposes.

For values which are allocated on the heap, `eq?` performs
a direct pointer comparison. For immediate values (like nil,
integers, floats) it is instead a direct tag-and-contents
comparison.

Note that these rules mean that technically, `2` and `2.0` are not
`eq?` to each other. It is once again recommended to use a more specific equality
function whenever you can be sure you need it - for this example,
it would be better to use `=?` (the numeric equality predicate)
instead.

Equality operators:

- `(eq? v1 v2)`: identity comparison
- `(=? v1 v2)`: numerical comparison, will "promote" numbers to check
for actual numeric equality
- `(string=? v1 v2)`: string comparison, direct byte-for-byte comparison.
may be reduced to vector operations if strings are changed to be
vectors later on.
- `(symbol=? v1 v2)`: symbol comparison. equivalent to `eq?` for symbols
- `(list=? l1 l2 p)`: unlike most equality predicates, takes a binary
predicate `(p v1 v2)`, and iterates over the two lists comparing each
element with the matching one in the other list. Evaluates to `'t`
if both lists are the same length, and `p` never returned `()`.
