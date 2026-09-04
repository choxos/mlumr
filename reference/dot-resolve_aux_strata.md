# Map `aux_by` onto the Stan `n_strata` switch

There are only ever two studies in an unanchored comparison, so
`".study"` means 2 and `"none"` means 1. `NULL` resolves to the
`".study"` default and therefore also gives 2; only `"none"` asks for a
single shared stratum. Named after multinma's argument so the concept
transfers, but deliberately not accepting `".trt"`: each study
contributes a single arm here, so stratifying by treatment and by study
are the same thing.

## Usage

``` r
.resolve_aux_strata(aux_by)
```

## Arguments

- aux_by:

  `NULL`, `".study"`, or `"none"`.

## Value

Integer number of baseline strata (1 or 2).
