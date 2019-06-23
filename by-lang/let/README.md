# LET language

Language that contains:

* Integer constants
* Integer subtraction
* Zero-value check
* Conditional branching on If
* Let-bound Variables

Run main function in the following format:

```
$ dune exec bin/ex.bc
>>> let x = 5 in
  if zero?(-(x, 1)) then 0 else 1

1
```

(The interpreter takes an empty line to denote the end of the input expression)

Alternatively, run tests defined in `test/test.ml` using the `dune runtest` command.
