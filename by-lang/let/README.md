# LET language

Language that supports:

* Integer constants
* Integer subtraction
* Zero-value check
* Conditional branching on If
* Let-bound Variables

Run main function in the following format:

```
$ dune exec bin/ex.bc
```

The interactive interpreter will start with a ">>> " prompt, take (potentially multi-line) user input, parse and evaluate as a single expression, and print the result.

The interpreter takes an empty line to denote the end of the input expression.

Example:

```
>>> let x = 5 in
  if zero?(-(x, 1)) then 0 else 1

1
```


Alternatively, run tests defined in `test/test.ml` using the `dune runtest` command.
