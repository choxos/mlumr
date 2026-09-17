# Map `aux_by` onto the Stan `n_strata` switch

`".study"` (and `NULL`, as in multinma) means one baseline per study, 2;
`"none"` means one shared baseline, 1. `".trt"` is refused: each study
contributes one arm, so it would be the same stratification.

## Usage

``` r
.resolve_aux_strata(aux_by)
```

## Arguments

- aux_by:

  `NULL`, `".study"`, or `"none"`.

## Value

Integer number of baseline strata (1 or 2).
