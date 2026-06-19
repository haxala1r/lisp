# Continuation Passing Style

The olisp compiler transforms source code into an intermediate
representation form in Continuation Passing Style (CPS).
All source code is transformed to use CPS, passing explicit
continuation parameters between functions.

For the rest of this document, I will assume basic familiarity
with CPS.

There are several consequences of this transformation. Some
are good, some are bad:

- All code being in continuation passing style means that
the runtime does not require a call stack. Functions
never return, they simply call their continuation parameters,
therefore a "return address" and thus a call stack are unnecessary.
- ... as a result of this, all function calls are also tail calls.
All recursion is tail recursion. Stack overflows are impossible:
your recursive functions may keep recursing as long as there is
memory. Infinitely if the function is tail recursive.
- However, this does come at a cost: the resulting program
must allocate additional closure objects to cope with the lack of
a call stack. The program does in fact still encode return addresses,
they simply live in the heap-allocated closures instead of an
implicit call stack.

These are the direct consequences of CPS, however there is one
additional reason for CPS: it makes advanced control operators
(in the particular case of olisp: `shift` and `reset`) easier
to implement. This buys an incredible amount of conceptual power
at a very reasonable implementation cost.

## Shift and Reset

Shift and Reset (first proposed by Olivier Danvy and Andrzej Filinski in
Abstracting Control (1990)) are important delimited control operators used to
implement many of the control abstractions in olisp.

The `reset` operator sets a limit for the continuation while the `shift` operator
"captures" or "reifies" the current continuation up to the delimiter.

