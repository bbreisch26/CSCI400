The purpose of this assignment is to introduce *evaluation* by
implementing features such as unary and binary operators, strings,
conditionals, console output, and automatic type conversion.

Instructions
============

1. Merge the updated starter code into your team's repository.

      git pull upstream

2. Complete the tasks below.

3. Test your code!

4. Push all above changes to your team repo in the github.com course
   organization.  Ensure that the code you want graded is in the
   master branch before the deadline.

Your goal in this lab will be to add OCaml code to complete the
functionality described in `src/lab6.ml`.  Skeleton code is provided,
and you will need to fill in the body of several functions. Each
location where you need to replace a placeholder expression with your
own code is marked with a `TODO` comment.

You can check your work by comparing your evaluator's output to that
of an existing JavaScript interpreter such as `nodejs` (installed as
`node` on some systems).

Here is the grammar used for this project (note that this is a subset
of JavaScript).

- **expression** *e* ::= *v* | *uop* *e* | *e* *bop* *e*
                | *e* `?` *e* `:` *e* | `console.log` `(` *e* `)`

- **value** *v* ::= *n* | *b* | *s* | `undefined`

- **unary operator** *uop* ::= `-` | `!`

- **binary operator** *bop* ::= `+` | `-` | `*` | `/` | `===` | `!==` | `<` | `<=` | `>` | `>=` | `&&` | `||`

- **number (float)** *n*

- **boolean** *b* ::= `true` | `false`

- **string** *s*


Task 1
------

In this part of the lab, we will begin building our JavaScript
interpreter.  We will start by building a function which evaluates
simple JavaScript *expressions*.  For example, when given a JavaScript
expression such as `1+1`, your code should return the value `2`.  Add
support for all the binary and unary operators listed in the above
grammar.

- Edit `lab6.ml` at the locations indicated by `TODO` comments, and
  complete the `eval` function as described.
- Add at least 5 new *non-trivial* unit tests for this function to the
  `eval_tests` list at the location indicated by the `TODO` comment.

Task 2
------

Add support for the console print operator. Evaluating a program such
as `console.log(e)` should evaluate expression `e`, and print the
resulting value.

Task 3
------

Add support for conditional expressions. This is JavaScript's "inline
*if*".  Add at least 3 unit tests for this conditional functionality
to the `cond_eval_tests` list (location marked with `TODO`).

Task 4
------

Add support for strings. Note that the `+`, `<`, `<=`, `>`, `>=`
operators work differently for strings versus numbers.  Add at least 3
unit tests for your string functionality to the `str_eval_tests` list
(location marked with `TODO`).

Documentation
--------------

- Please provide concise documentation in code comments for each of
  the features you implement.

Using Your Interpreter from the Command Line
--------------------------------------------

- Type `./lab6` to read from standard input.  If you are reading from
  the terminal, press CTRL-D when finished entering the expression.
- You can also pipe input into the program, e.g. `echo '1+2' |
  ./lab6`.
- Type `./lab6 file.js` to read input from a file named file.js.

References
==========

- JavaScript:
  - [JavaScript Standard](https://262.ecma-international.org/10.0/)
  - Mozilla's [JavaScript Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
  - [NodeJS](https://nodejs.org/) JavaScript Runtime
