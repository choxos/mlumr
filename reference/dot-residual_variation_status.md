# Classify how much residual variation a normal IPD outcome has

The decision core behind
[`.check_normal_residual_variation()`](https://choxos.github.io/mlumr/reference/dot-check_normal_residual_variation.md),
which turns the status into a refusal, a warning or silence. Kept
separate so the mixed-zero log-link case can classify the positive rows
on their own.

## Usage

``` r
.residual_variation_status(X, y, link)
```

## Arguments

- X:

  Design matrix as the model fits it, intercept included: raw, or
  centered by the model's own centers.

- y:

  Outcome vector; positive under `link = "log"`.

- link:

  `"identity"` or `"log"`.

## Value

List with `status` and, where they apply, `n`, `rank`, `numerical_rank`,
`ratio` and `zero_ratio`.

## Details

Everything here is about the design the model fits, `X`, whose exact
rank and exactly independent pivot columns come from
[`.exact_rank()`](https://choxos.github.io/mlumr/reference/dot-exact_rank.md).
A factorization at machine precision can find a lower rank, when a
column differs from a combination of the others by less than rounding;
the model still carries that column, and a fit through it can be exact
where the reduced fit shows a residual. So the numerical fit is taken on
the exact pivot columns, and if even those cannot be resolved the
question is left open rather than answered from a design the model does
not fit.

Statuses, in the order they are decided:

- `"saturated"`: `n <= rank`, the design reproduces any outcome.

- `"constant"`: the outcome is constant with residual degrees of
  freedom, so the intercept alone fits it exactly.

- `"exact"`: every replicate group agrees and there are exactly `rank`
  distinct design rows, so the design reaches every observed value.

- `"unresolved"`: the exact pivot columns are dependent to within
  rounding, so no numerical fit spans the design the model fits.

- `"unresolved_log"`: `log(y)` is constant although `y` is not, so the
  log-scale total sum of squares is zero and the ratio undefined.

- `"undecidable"`: the computed residual is at or below the rounding an
  exact fit can leave.

- `"near_exact"`: a real residual at most `1e-6` of the total.

- `"positive"`: a real residual.
