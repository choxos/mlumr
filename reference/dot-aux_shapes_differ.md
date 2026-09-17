# Does the baseline shape differ between strata?

`n_strata > 1` alone is not the question: the exponential has no shape
parameter to stratify, so `aux_by = ".study"` changes nothing there.
This mirrors the Stan gate `n_strata > 1 && nonexp && dist <= 3`; the
flexible models stratify their whole baseline.

## Usage

``` r
.aux_shapes_differ(object)
```

## Arguments

- object:

  An `mlumr_fit` (survival family).

## Value

`TRUE` when the strata have different baseline shapes.
