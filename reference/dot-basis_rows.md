# Select the rows of a spline basis that match selected prediction times

The basis matrices carry one row per fitted time, so a time and its row
are chosen together. `NULL` for a parametric fit, which evaluates
analytically and has no basis.

## Usage

``` r
.basis_rows(basis, idx)
```
