# The further arguments the installed loo reads for a log-likelihood matrix

Read off the formals of `loo.matrix` or `waic.matrix` in the installed
`loo`, less `x` and `r_eff`, which mlumr supplies. A fixed list would
accept an argument that some `loo` release may not define, `is_method`
for one, and that release would drop it through `...` without notice,
which is the failure the refusal exists to prevent.

## Usage

``` r
.loo_matrix_reads(generic)
```

## Arguments

- generic:

  `"loo"` or `"waic"`.

## Value

A character vector of argument names; empty if the method is not found.
