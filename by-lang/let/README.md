#LET language

Language that contains:

* Integer constants
* Integer subtraction
* Zero-value check
* Conditional branching on If
* Let-bound Variables

Run main function in the following format:

```
echo "let x = 5 in -(x, 4)" | dune exec bin/ex.bc
cat some_file_with_expr | dune exec bin/ex.bc
```

Alternatively, run tests defined in `test/test.ml` using the `dune runtest` command.
