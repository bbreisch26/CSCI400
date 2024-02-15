The purpose of this assignment is to help you understand algebraic
data types and persistent data structures.

Instructions
============

1. Do not re-clone this repository.  Merge the updated starter code
   into your team's repository as described in the main `README` file
   and Lab 0.

2. Part I: Implement the following functions in `lab3_exp.ml`:
   - `RPN.eval`: O(n), ~10 lines
   - `Exp.eval`: O(n), ~10 lines

3. Part II: Implement the following functions in `lab3_rbtree.ml`:
   - `RBTree.is_invariant`: O(n), ~15 lines
   - `RBTree.is_sorted`: O(n), ~15 lines
   - `RBTree.search`: O(h) = O(ln n), ~8 lines
   - `RBTree.balance`: O(1), ~10 lines
   - `RBTree.insert`: O(h) = O(ln n), ~12 lines
   - `RBTree.map_inorder`: O(n), ~6 lines
   - `RBTree.map_revorder`: O(n), ~6 lines

4. Part III: Implement the following functions in `lab3_rope.ml`:
   - `Rope.get`: O(h) = O(ln n), ~8 lines
   - `Rope.rot_right`: O(1), ~3 lines
   - `Rope.rot_right_left`: O(1), ~6 lines
   - `Rope.rot_left`: O(1), ~3 lines
   - `Rope.rot_left_right`: O(1), ~6 lines
   - `Rope.bal`: O(h) = O(ln n), ~15 lines
   - `Rope.sub`: O(h) = O(ln n), ~15 lines

5. Add at least 5 new *non-trivial* unit tests per function, at the
   locations marked by `TODO` comments.  Note that some functions are
   tested with multiple value types.

6. Test your code!

7. Push all above changes to your team repo in the github.com course
   organization.  Ensure that the code you want graded is in the
   master branch before the deadline.

Building and Testing
====================

- Type `make lab3` to build the lab
- Type `make lab3_test` to run the test cases defined in `src/lab3.ml`
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

You are NOT permitted to use:
- Loops (for, while)
- Mutable variables (ref)
- OCaml libraries, operators, or functions that directly implement the
  functions you must implement.  For example, you may not use any
  existing tree or set libraries.

References
==========
- Lecture 07: Algebraic Data Types
- Lecture 08: Persistence
- Okasaki. Ch 3.3 Red-Black Trees
- [Clarkson. Ch 8.3: Red-Black Trees]
  (https://cs3110.github.io/textbook/chapters/ds/rb.html)
- Boehm, H.J., Atkinson, R. and Plass, M., 1995. Ropes: an alternative
  to strings. Software: Practice and Experience, 25(12),
  pp.1315-1330. https://doi.org/10.1002/spe.4380251203
