The purpose of this assignment is to understand lazy evaluation and
amortization of persistent data structure.

Instructions
============

1. Do not re-clone this repository.  Merge the updated starter code
   into your team's repository as described in the main `README` file
   and Lab 0.

2. Part I: Implement the following functions in `lab4_lazy.ml`.
   - `force`: ~5 lines

3. Part II: Implement the following functions in `lab4_stream.ml`.
   - `Stream.head`: ~3 lines
   - `Stream.tail`: ~3 lines
   - `Stream.from_list`: ~5 lines
   - `Stream.fold_right`: ~3 lines
   - `Stream.seed`: ~1 line
   - `Stream.seed2`: ~2 lines
   - `Stream.append`: ~5 lines
   - `Stream.reverse`: ~5 lines
   - `Stream.prefix`: ~5 lines
   - `Stream.map`: ~5 lines
   - `Stream.map2`: ~7 lines
   - `Stream.filter`: ~10 lines

4. Part III: Implement the following functions in `lab4_queue.ml`.
   - `Queue.rotate`: ~12 lines.  Think carefully about the lazy
     evaluation in this function.  A naive implementation by produce
     correct values, but incorrect running times!
   - `Queue.check`: ~4 lines
   - `Queue.snoc`: ~2 lines
   - `Queue.head`: ~4 lines
   - `Queue.tail`: ~4 lines
   - `Queue.fold_right`: ~3 lines

5. Add at least 3 new *non-trivial* unit tests per function, at the
   locations marked by `TODO` comments.

6. Test your code!

7. Push all above changes to your team repo in the github.com course
   organization.  Ensure that the code you want graded is in the
   master branch before the deadline.

Building and Testing
====================

- Type `make lab4` to build the lab
- Type `make lab4_test` to run the test cases defined in `src/lab4.ml`
- Each unit test is a tuple of the form `(optional_name, input,
  expected_output)`, where `optional_name` can either be `None` or
  `Some(x)`, where `x` is a human-readable name for the unit test. The
  `expected_output` is a `result` type, which allows you to use
  `Ok(out)` for regular output `out`, or `Error(ex)` if the unit test
  is expected to generate an exception `ex`.

Allowable OCaml Subset
======================

To help you learn to think in the functional programming style, this
project is restricted to a subset of OCaml.

You are permitted to use:
- binding (let)
- function definition (let, fun, let rec)
- function application (f x)
- pattern matching (match e with ...)
- conditionals (if e1 then e2 else e3)
- cons and list construction (a::d and [...])
- tuples ((...))
- arithmetic, Boolean, and equality operations (+,-,/,*,=, etc.)
- options (Some X, None)
- List.rev, only in Queue.rotate

Within `lab4_lazy.ml`, you are permitted to use references to
implement memoization.  You may not use references for anything else.

You are NOT permitted to use:
- Loops (for, while)
- Mutable variables (ref), except to implement memoization
- OCaml libraries, operators, or functions that directly implement the
  functions you must implement.  For example, you may not use any
  existing lazy evaluation, stream, or queue libraries.

References
==========
- Lecture 10: Lazy Evaluation
- Lecture 12: Persistent Amortization via Laziness
- Okasaki. Ch 4 Lazy Evaluation
- Okasaki. Ch 6 Amortization and Persistence via Lazy Evaluation
